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
  private
    FCmd: string;
    FArg: TArray<string>;
    FEnv: TArray<string>;
    FStartupInfo: TStartupInfo;
    FProcessInfo: TProcessInformation;
    //Job
    FJob: THandle;
    //Std reader and writer
    FStdIn: IStdWriter;
    FStdOut: IStdReader;
    FStdErr: IStdReader;
  private
    function GetStatus: cardinal;
    procedure AfterCreateSubprocess(const APId: integer);
    procedure Execute();
  protected
    function GetStdOut(): IStdReader;
    function GetStdIn(): IStdWriter;
    function GetStdErr(): IStdReader;
    function GetOutputBytes(): TBytes;
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

    function Config(const AExecCmd: TProc<IExecCmd>): IExecCmd;

    function Run(): IExecCmd; overload;
    function Run(out AOutput: TBytes): IExecCmd; overload;
    function Run(out AOutput: string): IExecCmd; overload;
    function Run(const ARedirections: TRedirections): IExecCmd; overload;

    procedure Kill();
    function Wait(): Integer;

    class function GetEnvironmentVariables(): TArray<string>;

    property Status: cardinal read GetStatus;
    property IsAlive: boolean read GetIsAlive;
    property ExitCode: Integer read GetExitCode;
  end;

const
  cCANCELATION_SIGNAL_EXIT_CODE = $001A;

implementation

uses
  System.Math,
  System.SyncObjs,
  PyTools.ExecCmd.StdIO,
  PyTools.ExecCmd.StdIO.Null,
  PyTools.ExecCmd.StdIO.Pipe;

{ TExecCmd }

procedure TExecCmd.AfterCreateSubprocess(const APId: integer);
begin
  FStdIn.PID := APId;
  FStdOut.PID := APId;
  FStdErr.PID := APId;
end;

function TExecCmd.Config(const AExecCmd: TProc<IExecCmd>): IExecCmd;
begin
  if Assigned(AExecCmd) then
    AExecCmd(Self);
  Result := Self;
end;

constructor TExecCmd.Create(const ACmd: string; const AArg, AEnv: TArray<string>);
begin
  inherited Create();
  FCmd := ACmd;
  FArg := AArg;
  FEnv := AEnv;

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
  FStdOut.ReadAll(LOutput, INFINITE);
  Result := LOutput;
  FStdErr.ReadAll(LOutput, INFINITE);
  Result := Result + LOutput;
end;

function TExecCmd.GetOutputBytes: TBytes;
var
  LOutput: TBytes;
begin
  LOutput := nil;
  FStdOut.ReadAllBytes(LOutput, INFINITE);
  Result := LOutput;
  FStdErr.ReadAllBytes(LOutput, INFINITE);
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

  AfterCreateSubprocess(FProcessInfo.hProcess);
end;

procedure TExecCmd.RedirectPipes(const ARedirections: TRedirections);
begin
  if (TRedirect.stdin in ARedirections) then
    FStdIn := TPipeStdWriter.Create(STD_INPUT_HANDLE)
  else
    FStdIn := TNullStdWriter.Create(STD_INPUT_HANDLE);

  if (TRedirect.stdout in ARedirections) then
    FStdOut := TPipeStdReader.Create(STD_OUTPUT_HANDLE)
  else
    FStdOut := TNullStdReader.Create(STD_OUTPUT_HANDLE);

  if (TRedirect.stderr in ARedirections) then
    FStdErr := TPipeStdReader.Create(STD_ERROR_HANDLE)
  else
    FStdErr := TNullStdReader.Create(STD_ERROR_HANDLE);

  FStartupInfo.hStdInput := FStdIn.PipeDescriptors.ReadDes;
  FStartupInfo.hStdOutput := FStdOut.PipeDescriptors.WriteDes;
  FStartupInfo.hStdError := FStdErr.PipeDescriptors.WriteDes;
end;

function TExecCmd.Run(const ARedirections: TRedirections): IExecCmd;
begin
  RedirectPipes(ARedirections);
  Execute();
  Result := Self;
end;

function TExecCmd.Run(out AOutput: TBytes): IExecCmd;
begin
  Result := Run([TRedirect.stdout, TRedirect.stderr]);
  AOutput := GetOutputBytes();
end;

function TExecCmd.Run(out AOutput: string): IExecCmd;
begin
  Result := Run([TRedirect.stdout, TRedirect.stderr]);
  AOutput := GetOutput();
end;

function TExecCmd.Run: IExecCmd;
begin
  var LRedirections: TRedirections := [];
  Result := Run(LRedirections);
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

end.
