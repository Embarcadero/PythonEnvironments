(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyTools.ExecCmd.Windows'                                *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality: Execute Shell Commands and/or Subprocess               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)

{$IFNDEF PLATFORM_UNIT}
unit PyTools.ExecCmd.Win;
{$ENDIF PLATFORM_UNIT}

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, PyTools.ExecCmd;

//Ref: https://docs.microsoft.com/en-us/windows/win32/procthread/creating-a-child-process-with-redirected-input-and-output
type
  TExecCmd = class(TInterfacedObject, IExecCmd)
  private type
    TStdBase = class(TInterfacedObject)
    protected
      Parent: TExecCmd;
      ReadPipe: THandle;
      WritePipe: THandle;
    public
      constructor Create(const AParent: TExecCmd; const AReadPipe, AWritePipe: THandle);
    end;

    TStdReader = class(TStdBase, IStdReader)
    private
      function PeekMessage(): string;
    public
      function ReadNext: string;
      function ReadAll(): string; overload;
      function ReadAll(out AValue: string; const ATimeout: cardinal): boolean; overload;
    end;

    TStdWriter = class(TStdBase, IStdWriter)
    private
      procedure PushStdIn(const AValue: string);
    public
      procedure Write(const AValue: string);
    end;
  private
    FCmd: string;
    FArg: TArray<string>;
    FEnv: TArray<string>;
    FSecurityAttributes: TSecurityAttributes;
    FStartupInfo: TStartupInfo;
    FProcessInfo: TProcessInformation;
    //Pipes
    //StdOut
    FStdOutPipeRead: THandle;
    FStdOutPipeWrite: THandle;
    //StdIn
    FStdInPipeRead: THandle;
    FStdInPipeWrite: THandle;
    //StdErr
    FStdErrPipeRead: THandle;
    FStdErrPipeWrite: THandle;
    //Job
    FJob: THandle;
    //Std reader and writer
    FStdIn: IStdWriter;
    FStdOut: IStdReader;
    FStdErr: IStdReader;
  private
    function GetStatus: cardinal;
    procedure Execute();
  protected
    function GetStdOut(): IStdReader;
    function GetStdIn(): IStdWriter;
    function GetStdErr(): IStdReader;
    function GetOutput(): string;
    function GetIsAlive: boolean;
    function GetExitCode: Integer;
    procedure RedirectPipes(const ARedirections: TRedirections);
    // EnvP
    function CreateEnvBlock(const AEnvP: TArray<string>;
      const ABuffer: pointer; const ABufferSize: integer): integer;
  public
    constructor Create(const ACmd: string; const AArg, AEnv: TArray<string>);
    destructor Destroy(); override;
    function Run(): IExecCmd; overload;
    function Run(out AOutput: string): IExecCmd; overload;
    function Run(const ARedirections: TRedirections): IExecCmd; overload;
    function Wait(): Integer;
    procedure Kill();
    class function GetEnvironmentVariables(): TArray<string>;
    property Status: cardinal read GetStatus;
    property IsAlive: boolean read GetIsAlive;
    property ExitCode: Integer read GetExitCode;
  end;

const
  cCANCELATION_SIGNAL_EXIT_CODE = $001A;

implementation

uses
  System.SyncObjs,
  Math;

{ TExecCmd }

constructor TExecCmd.Create(const ACmd: string; const AArg, AEnv: TArray<string>);
begin
  inherited Create();
  FCmd := ACmd;
  FArg := AArg;
  FEnv := AEnv;
  //Set the bInheritedHandle to true so pipe handles are inherited
  FSecurityAttributes := Default(TSecurityAttributes);
  with FSecurityAttributes do begin
    nLength := SizeOf(FSecurityAttributes);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;

  //Create the startup information to the process
  FStartupInfo := Default(TStartupInfo);
  with FStartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    wShowWindow := SW_HIDE;
  end;

  //Create a job that kills the subprocess when parent dies
  FJob := CreateJobObject(nil, nil);
  if (FJob <> 0) then begin
    var LExInfo: TJobObjectExtendedLimitInformation;
    LExInfo.BasicLimitInformation.LimitFlags := JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if not SetInformationJobObject(FJob, JobObjectExtendedLimitInformation,
      @LExInfo, SizeOf(TJobObjectExtendedLimitInformation)) then
        RaiseLastOSError();
  end;

  // Close handles to the stdin and stdout pipes no longer needed by the child process.
  // If they are not explicitly closed, there is no way to recognize that the child process has ended.
  CloseHandle(FStdOutPipeWrite);
  CloseHandle(FStdErrPipeWrite);
  CloseHandle(FStdInPipeRead);
end;

destructor TExecCmd.Destroy;
begin
  if IsAlive then
    Kill();
  CloseHandle(FProcessInfo.hThread);
  CloseHandle(FProcessInfo.hProcess);
  inherited;
end;

function TExecCmd.CreateEnvBlock(const AEnvP: TArray<string>;
  const ABuffer: pointer; const ABufferSize: integer): integer;
begin
  if not Assigned(FEnv) then
    Exit(0);

  Result := 0;
  for var I := Low(AEnvP) to High(AEnvP) do
    Inc(Result, Length(AEnvP[I]) + 1);
  Inc(Result);

  if (ABuffer <> nil) and (ABufferSize >= Result) then begin
    var PBuf := PChar(ABuffer);
    for var I := Low(AEnvP) to High(AEnvP) do begin
      StrPCopy(PBuf, AEnvP[I]);
      Inc(PBuf, Length(AEnvP[I]) + 1);
    end;
    // Terminate block with additional #0
    PBuf^ := #0;
  end;
end;

class function TExecCmd.GetEnvironmentVariables: TArray<string>;
begin
  var LEnvP: PChar := GetEnvironmentStrings();
  if not Assigned(LEnvP) then
    Exit(nil);

  // EnvP strings are #0 separated and list ends with #0#0
  Result := nil;
  var LEnvPEntry: PChar := LEnvP;
  try
    while LEnvPEntry^ <> #0 do begin
      Result := Result + [LEnvPEntry];
      Inc(LEnvPEntry, StrLen(LEnvPEntry) + 1);
    end;
  finally
    FreeEnvironmentStrings(LEnvP);
  end;
end;

function TExecCmd.GetExitCode: Integer;
begin
  if not GetExitCodeProcess(FProcessInfo.hProcess, Cardinal(Result)) then
    RaiseLastOSError();
end;

function TExecCmd.GetIsAlive: boolean;
var
  LExitCode: cardinal;
begin
  GetExitCodeProcess(FProcessInfo.hProcess, LExitCode);
  Result := (LExitCode = STILL_ACTIVE);
end;

function TExecCmd.GetOutput: string;
var
  LOutput: string;
begin
  LOutput := String.Empty;
  if Assigned(FStdOut) then
    FStdOut.ReadAll(LOutput, INFINITE);
  Result := LOutput;
  if Assigned(FStdErr) then
    FStdErr.ReadAll(LOutput, INFINITE);
  Result := Result + LOutput;
end;

function TExecCmd.GetStatus: cardinal;
begin
  Result := WaitForSingleObject(FProcessInfo.hProcess, 0);
end;

function TExecCmd.GetStdIn: IStdWriter;
begin
  Result := FStdIn;
end;

function TExecCmd.GetStdOut: IStdReader;
begin
  Result := FStdOut;
end;

function TExecCmd.GetStdErr: IStdReader;
begin
  Result := FStdErr;
end;

procedure TExecCmd.Execute;
begin
  //Create the process
  var LCmd := FCmd + ' ' + String.Join(' ', FArg);
  UniqueString(LCmd);

  var LEnvP := nil;
  var LCreateFlags := 0;
  if Assigned(FEnv) then begin
    var LBlockSize := CreateEnvBlock(FEnv, nil, 0);
    if LBlockSize > 0 then begin
      LEnvP := StrAlloc(LBlockSize);
      CreateEnvBlock(FEnv, LEnvP, LBlockSize);
      LCreateFlags := CREATE_UNICODE_ENVIRONMENT;
    end;
  end;

  if not CreateProcess(
    nil,
    PWideChar(LCmd), // Process + Argument values
    nil, nil, True,
    LCreateFlags, // CreateFlags
    LEnvP, // Environment parameters
    nil,
    FStartupInfo, FProcessInfo) then
      RaiseLastOSError();

  //Assign the process to the job. It takes the proc. down when parent is killed.
  AssignProcessToJobObject(FJob, FProcessInfo.hProcess);
end;

procedure TExecCmd.RedirectPipes(const ARedirections: TRedirections);
begin
  //STD OUT REDIRECTION
  if (TRedirect.stdout in ARedirections) then begin
    if not CreatePipe(FStdOutPipeRead, FStdOutPipeWrite, @FSecurityAttributes, 0) then
      RaiseLastOSError();
    //Ensure the read handle to the pipe for STDOUT is not inherited
    //if not SetHandleInformation(FStdOutPipeRead, HANDLE_FLAG_INHERIT, 0) then
    //  RaiseLastOSError();
    //Redirect to our pipe
    FStartupInfo.hStdOutput := FStdOutPipeWrite;
  end else begin
    FStartupInfo.hStdOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  end;

  //STD ERR REDIRECTION
  if (TRedirect.stderr in ARedirections) then begin
    if not CreatePipe(FStdErrPipeRead, FStdErrPipeWrite, @FSecurityAttributes, 0) then
      RaiseLastOSError();
    //Ensure the read handle to the pipe for STDOUT is not inherited
    //if not SetHandleInformation(FStdOutPipeRead, HANDLE_FLAG_INHERIT, 0) then
    //  RaiseLastOSError();
    //Redirect to our pipe
    FStartupInfo.hStdError := FStdErrPipeWrite;
  end else begin
    FStartupInfo.hStdError := GetStdHandle(STD_ERROR_HANDLE);
  end;

  //STD IN REDIRECTION
  if (TRedirect.stdin in ARedirections) then begin
    //Create a pipe for the child process's STDIN
    if not CreatePipe(FStdInPipeRead, FStdInPipeWrite, @FSecurityAttributes, 0) then
      RaiseLastOSError();
    //Ensure the write handle to the pipe for STDIN is not inherited
    //if not SetHandleInformation(FStdInPipeWrite, HANDLE_FLAG_INHERIT, 0) then
    //  RaiseLastOSError();
    //Redirect to our pipe
    FStartupInfo.hStdInput := FStdInPipeRead;
  end else begin
    FStartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  end;
end;

function TExecCmd.Run(const ARedirections: TRedirections): IExecCmd;
begin
  RedirectPipes(ARedirections);
  if (TRedirect.stdout in ARedirections) then
    FStdOut := TStdReader.Create(Self, FStdOutPipeRead, FStdOutPipeWrite);
  if (TRedirect.stderr in ARedirections) then
    FStdErr := TStdReader.Create(Self, FStdErrPipeRead, FStdErrPipeWrite);
  if (TRedirect.stdin in ARedirections) then
    FStdIn := TStdWriter.Create(Self, FStdInPipeRead, FStdInPipeWrite);
  Execute();
  Result := Self;
end;

function TExecCmd.Run(out AOutput: string): IExecCmd;
begin
  Result := Run([TRedirect.stdout, TRedirect.stderr]);
  AOutput := GetOutput();
end;

function TExecCmd.Run: IExecCmd;
begin
  Result := Run([]);
end;

function TExecCmd.Wait: Integer;
begin
  TSpinWait.SpinUntil(function(): boolean begin
    Result := not GetIsAlive();
  end, INFINITE);
  Result := GetExitCode();
end;

procedure TExecCmd.Kill;
begin
  if (Status = WAIT_TIMEOUT) then
    if not TerminateProcess(FProcessInfo.hProcess, cCANCELATION_SIGNAL_EXIT_CODE) then
      RaiseLastOSError();
end;

{ TExecCmd.TStdBase }

constructor TExecCmd.TStdBase.Create(const AParent: TExecCmd; const AReadPipe,
  AWritePipe: THandle);
begin
  inherited Create();
  Parent := AParent;
  ReadPipe := AReadPipe;
  WritePipe := AWritePipe;
end;

{ TExecCmd.TStdReader }

function TExecCmd.TStdReader.PeekMessage(): string;
const
  BUFFSIZE = 4096;
type
  TBuffArr = array[0..BUFFSIZE - 1] of AnsiChar;
var
  LBytesRead: cardinal;
  LBuffer: TBuffArr;
begin
  if not PeekNamedPipe(ReadPipe, nil, 0, nil, @LBytesRead, nil) or (LBytesRead = 0) then
    Exit(String.Empty);
  if not ReadFile(ReadPipe, LBuffer, BUFFSIZE, LBytesRead, nil) then
    RaiseLastOSError();
  if (LBytesRead > 0) then begin
    SetString(Result, LBuffer, LBytesRead);
  end;
end;

function TExecCmd.TStdReader.ReadAll(out AValue: string;
  const ATimeout: cardinal): boolean;
var
  LValue: string;
begin
  LValue := String.Empty;
  Result := TSpinWait.SpinUntil(
    function(): boolean
    var
      LBuffer: string;
    begin
      LBuffer := PeekMessage();
      if not LBuffer.IsEmpty() then
        LValue := LValue + LBuffer;
       //Let's read it until it dies
       Result := not Parent.IsAlive;
       //Read until queue is empty, even if it is dead
       if Result then
         Result := LBuffer.IsEmpty();
    end, ATimeout);
  AValue := LValue;
end;

function TExecCmd.TStdReader.ReadNext: string;
begin
  Result := PeekMessage();
end;

function TExecCmd.TStdReader.ReadAll: string;
begin
  while not ReadAll(Result, INFINITE) do;
end;

{ TExecCmd.TStdWriter }

procedure TExecCmd.TStdWriter.PushStdIn(const AValue: string);
var
  LIn: AnsiString;
  LBytesWritten: cardinal;
begin
  LIn := AnsiString(AValue);
  WriteFile(WritePipe, LIn[1], Length(LIn), LBytesWritten, nil);
end;

procedure TExecCmd.TStdWriter.Write(const AValue: string);
begin
  PushStdIn(AValue);
end;

end.
