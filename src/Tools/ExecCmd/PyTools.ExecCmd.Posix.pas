(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyTools.ExecCmd.Posix'                                  *)
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
unit PyTools.ExecCmd.Posix;
{$ENDIF PLATFORM_UNIT}

interface

uses
  System.SysUtils,
  Posix.Base, Posix.Fcntl, Posix.Unistd, Posix.SysWait, Posix.Stdlib,
  Posix.Stdio, Posix.SysTypes, Posix.Signal, Posix.Errno, Posix.SysStat,
  Posix.String_,
  PyTools.ExecCmd;

type
  TStreamHandle = pointer;

  TExecCmd = class(TInterfacedObject, IExecCmd)
  private type
    TStdBase = class(TInterfacedObject)
    protected
      Parent: TExecCmd;
      PipeDescriptor: TPipeDescriptors;
    public
      constructor Create(const AParent: TExecCmd; const APipeDescriptor: TPipeDescriptors);
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
    FPid: Integer;
    FStdOutPipe: TPipeDescriptors;
    FStdErrPipe: TPipeDescriptors;
    FStdInPipe: TPipeDescriptors;
    FExitCode: integer;
    //Std reader and writer
    FStdIn: IStdWriter;
    FStdOut: IStdReader;
    FStdErr: IStdReader;
  protected
    function GetStdOut(): IStdReader;
    function GetStdIn(): IStdWriter;
    function GetStdErr(): IStdReader;
    function GetOutput(): string;
    function GetIsAlive: boolean;
    function GetExitCode: Integer;

    procedure RedirectPipes(const ARedirections: TRedirections);
  public
    constructor Create(const ACmd: string; AArg, AEnv: TArray<string>);
    destructor Destroy(); override;

    function Run(): IExecCmd; overload;
    function Run(out AOutput: string): IExecCmd; overload;
    function Run(const ARedirections: TRedirections): IExecCmd; overload;

    procedure Kill();
    function Wait(): Integer;

    property IsAlive: boolean read GetIsAlive;
    property ExitCode: Integer read GetExitCode;
  end;

implementation

uses
  System.IOUtils,
  System.SyncObjs,
  PyTools.Exception;

const
  INITIAL_EXIT_CODE = -999;

{ TExecCmd }

constructor TExecCmd.Create(const ACmd: string; AArg, AEnv: TArray<string>);
begin
  inherited Create();
  FCmd := ACmd;
  FArg := AArg;
  FEnv := AEnv;
  FExitCode := INITIAL_EXIT_CODE;
end;

destructor TExecCmd.Destroy;
begin
  if IsAlive then
    Kill();

  __close(FStdOutPipe.ReadDes);
  __close(FStdErrPipe.ReadDes);
  __close(FStdInPipe.WriteDes);
  inherited;
end;

function TExecCmd.GetExitCode: Integer;
begin
  Result := FExitCode;
end;

function TExecCmd.GetIsAlive: boolean;
var
  LWaitedPid: integer;
  LStatus: integer;
begin
  if (FExitCode <> INITIAL_EXIT_CODE) then
    Exit(false);
    
  LWaitedPid := waitpid(FPid, @LStatus, WNOHANG);
  if (LWaitedPid = -1) then
    raise EWaitFailed.Create('Failed waiting for proess.')
  else if (LWaitedPid = 0) then
    Exit(true)
  else begin    
    if WIFEXITED(LStatus) then begin
      FExitCode := WEXITSTATUS(LStatus);
    end else if WIFSIGNALED(LStatus) then begin
      FExitCode := WTERMSIG(LStatus);
    end else if WIFSTOPPED(LStatus) then begin
      FExitCode := WSTOPSIG(LStatus);
    end else begin
      FExitCode := EXIT_FAILURE;
    end;
  end;
  Result := false;
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

function TExecCmd.GetStdOut: IStdReader;
begin
  Result := FStdOut;
end;

function TExecCmd.GetStdErr: IStdReader;
begin
  Result := FStdErr;
end;

function TExecCmd.GetStdIn: IStdWriter;
begin
  Result := FStdIn;
end;

procedure TExecCmd.RedirectPipes(const ARedirections: TRedirections);
var
  LMarshaller: TMarshaller;
  LArg, LEnv: array of PAnsiChar;
  I: Integer;
begin
  //#define PARENT_READ read_pipe[0]
  //#define PARENT_WRITE write_pipe[1]
  //#define CHILD_WRITE read_pipe[1]
  //#define CHILD_READ  write_pipe[0]

  if (pipe(FStdOutPipe) = -1) or (pipe(FStdErrPipe) = -1) or (pipe(FStdInPipe) = -1) then
    raise EPipeFailed.Create('Failed to create pipe.');

  FPid := fork();
  if (FPid < 0) then
    raise EForkFailed.Create('Failed to fork process.')
  else if (FPid = 0) then begin
    while ((dup2(FStdOutPipe.WriteDes, STDOUT_FILENO) = -1) and (errno = EINTR)) do;
    while ((dup2(FStdErrPipe.WriteDes, STDERR_FILENO) = -1) and (errno = EINTR)) do;
    while ((dup2(FStdInPipe.ReadDes, STDIN_FILENO) = -1) and (errno = EINTR)) do;
    __close(FStdOutPipe.WriteDes);
    __close(FStdOutPipe.ReadDes);
    __close(FStdErrPipe.WriteDes);
    __close(FStdErrPipe.ReadDes);
    __close(FStdInPipe.ReadDes);

    //https://man7.org/linux/man-pages/man2/execve.2.html

    //argv is an array of pointers to strings passed to the new program
    //as its command-line arguments. By convention, THE FIRST OF THESE
    //STRINGS (i.e., argv[0]) SHOULD CONTAIN THE FILENAME ASSOCIATED
    //WITH THE FILE BEING EXECUTED. The argv array must be terminated
    //by a NULL pointer. (Thus, in the new program, argv[argc] will be
    //NULL.)

    SetLength(LArg, Length(FArg) + 1);
    for I := Low(FArg) to High(FArg) do
      LArg[I] := LMarshaller.AsAnsi(PWideChar(FArg[I]) + #0).ToPointer();
    LArg[High(LArg)] := PAnsiChar(nil);

    SetLength(LEnv, Length(FEnv) + 1);
    for I := Low(FEnv) to High(FEnv) do
      LEnv[I] := LMarshaller.AsAnsi(PWideChar(FEnv[I]) + #0).ToPointer();
    LEnv[High(LEnv)] := PAnsiChar(nil);

    if execve(LMarshaller.AsAnsi(PWideChar(FCmd)).ToPointer(), PPAnsiChar(LArg), PPAnsiChar(LEnv)) = -1 then begin
      Halt(errno);
    end else
      Halt(EXIT_FAILURE);
  end else if (FPid > 0) then begin
    __close(FStdOutPipe.WriteDes);
    __close(FStdErrPipe.WriteDes);
    __close(FStdInPipe.ReadDes);
    __close(FStdInPipe.WriteDes);
  end;
end;

function TExecCmd.Run(const ARedirections: TRedirections): IExecCmd;
begin
  RedirectPipes(ARedirections);

  if (TRedirect.stdout in ARedirections) then
    FStdOut := TStdReader.Create(Self, FStdOutPipe);

  if (TRedirect.stderr in ARedirections) then
    FStdErr := TStdReader.Create(Self, FStdErrPipe);

  if (TRedirect.stdin in ARedirections) then
    FStdIn := TStdWriter.Create(Self, FStdInPipe);

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

procedure TExecCmd.Kill;
begin
  if (Posix.Signal.kill(FPid, Posix.Signal.SIGKILL) <> 0) then
    if (errno = EINVAL) then //Invalid signal
      raise EInvalidArgument.Create('Invalid argument.')
    else if (errno = EPERM) then //The process does not have permission to send the signal to any of the target processes.
      raise EOperationNotPermitted.Create('Operation not permitted.') 
    else if (errno = ESRCH) then //The pid or process group does not exist. Note that an existing process might be a zombie, a process which already committed termination, but has not yet been wait(2)ed for.
      raise ENoSuchProcess.Create('No such process.') 
end;

function TExecCmd.Wait: Integer;
begin
  TSpinWait.SpinUntil(function(): boolean begin
    Result := not GetIsAlive();
  end, INFINITE);

  Result := GetExitCode();
end;

{ TExecCmd.TStdBase }

constructor TExecCmd.TStdBase.Create(const AParent: TExecCmd;
  const APipeDescriptor: TPipeDescriptors);
begin
  inherited Create();
  Parent := AParent;
  PipeDescriptor := APipeDescriptor;
end;

{ TExecCmd.TStdReader }

function TExecCmd.TStdReader.PeekMessage: string;
var
  LBuffer: array[0..511] of UInt8;
  LCount: integer;
begin
  while True do begin
    LCount := __read(PipeDescriptor.ReadDes, @LBuffer[0], SizeOf(LBuffer));
    if (LCount = -1) then begin
      if (errno = EINTR) then
        Continue
      else
        Exit(String.Empty);
    end else if (LCount = 0) then
      Exit(String.Empty)
    else begin
      Exit(Copy(UTF8ToString(@LBuffer[0]), 1, UTF8ToString(@LBuffer[0]).Length -1));
    end;
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

function TExecCmd.TStdReader.ReadAll: string;
begin
  while not ReadAll(Result, INFINITE) do;
end;

function TExecCmd.TStdReader.ReadNext: string;
begin
  Result := PeekMessage();
end;

{ TExecCmd.TStdWriter }

procedure TExecCmd.TStdWriter.PushStdIn(const AValue: string);
var
  LMarshaller: TMarshaller;
begin
  __write(PipeDescriptor.WriteDes,
    LMarshaller.AsUtf8(PWideChar(AValue)).ToPointer(), AValue.Length);
end;

procedure TExecCmd.TStdWriter.Write(const AValue: string);
begin
  PushStdIn(AValue);
end;

end.
