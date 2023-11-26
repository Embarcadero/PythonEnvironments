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
  Posix.String_, Posix.Dlfcn,
  PyTools.ExecCmd;

type
  TStreamHandle = pointer;

  TExecCmd = class(TInterfacedObject, IExecCmd)
  private  
    FCmd: string;
    FArg: TArray<string>;
    FEnv: TArray<string>;
    FPid: Integer;
    FExitCode: integer;
    //Std reader and writer
    FStdIn: IStdWriter;
    FStdOut: IStdReader;
    FStdErr: IStdReader;
  private
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
  public
    constructor Create(const ACmd: string; AArg, AEnv: TArray<string>);
    destructor Destroy(); override;

    function Config(const AExecCmd: TProc<IExecCmd>): IExecCmd;

    function Run(): IExecCmd; overload;
    function Run(out AOutput: TBytes): IExecCmd; overload;
    function Run(out AOutput: string): IExecCmd; overload;
    function Run(const ARedirections: TRedirections): IExecCmd; overload;

    procedure Kill();
    function Wait(): Integer;

    class function GetEnvironmentVariables(): TArray<string>;

    property IsAlive: boolean read GetIsAlive;
    property ExitCode: Integer read GetExitCode;
  end;

implementation

uses
  System.Rtti,
  System.IOUtils,
  System.SyncObjs,
  System.Generics.Collections,
  PyTools.Exception,
  PyTools.ExecCmd.StdIO,
  PyTools.ExecCmd.StdIO.Null,
  PyTools.ExecCmd.StdIO.Pipe;

const
  INITIAL_EXIT_CODE = -999;

{ TExecCmd }

function TExecCmd.Config(const AExecCmd: TProc<IExecCmd>): IExecCmd;
begin
  if Assigned(AExecCmd) then
    AExecCmd(Self);
  Result := Self;
end;

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
  inherited;
end;

procedure TExecCmd.Execute;
var
  LMarshaller: TMarshaller;
  LArg, LEnv: array of PAnsiChar;
  I: Integer;
begin
  FPid := fork();

  if (FPid < 0) then
    raise EForkFailed.Create('Failed to fork process.');

  AfterCreateSubprocess(FPId);

  if (FPid = 0) then begin
    //https://man7.org/linux/man-pages/man2/execve.2.html
    //argv is an array of pointers to strings passed to the new program
    //as its command-line arguments. By convention, THE FIRST OF THESE
    //STRINGS (i.e., argv[0]) SHOULD CONTAIN THE FILENAME ASSOCIATED
    //WITH THE FILE BEING EXECUTED. The argv array must be terminated
    //by a NULL pointer. (Thus, in the new program, argv[argc] will be
    //NULL.)

    // Child process from here

    // Make argv
    SetLength(LArg, Length(FArg) + 1);
    for I := Low(FArg) to High(FArg) do
      LArg[I] := LMarshaller.AsAnsi(PWideChar(FArg[I]) + #0).ToPointer();
    LArg[High(LArg)] := PAnsiChar(nil);

    // Make envp
    SetLength(LEnv, Length(FEnv) + 1);
    for I := Low(FEnv) to High(FEnv) do
      LEnv[I] := LMarshaller.AsAnsi(PWideChar(FEnv[I]) + #0).ToPointer();
    LEnv[High(LEnv)] := PAnsiChar(nil);

    // execve requires the full path of the file being executed
    // enhance this implementation following https://man7.org/linux/man-pages/man3/execvp.3.html
    if execve(LMarshaller.AsAnsi(PWideChar(FCmd)).ToPointer(), PPAnsiChar(LArg), PPAnsiChar(LEnv)) = -1 then begin
      Halt(errno);
    end else
      Halt(EXIT_FAILURE);
  end else if (FPid > 0) then begin
    // Parent process from here
  end;
end;

class function TExecCmd.GetEnvironmentVariables: TArray<string>;
begin
  Result := nil;

  var LEnviron := environ();
  while Assigned(LEnviron) do begin
    Result := Result + [String(LEnviron^)];
    Inc(LEnviron);
  end;
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
    raise EWaitFailed.Create('Failed waiting for process.');

  if (LWaitedPid = 0) then
    Exit(true);

  if WIFEXITED(LStatus) then
    FExitCode := WEXITSTATUS(LStatus)
  else if WIFSIGNALED(LStatus) then
    FExitCode := WTERMSIG(LStatus)
  else if WIFSTOPPED(LStatus) then
    FExitCode := WSTOPSIG(LStatus)
  else
    FExitCode := EXIT_FAILURE;

  Result := false;
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

procedure TExecCmd.AfterCreateSubprocess(const APId: integer);
begin
  FStdIn.PID := APId;
  FStdOut.PID := APId;
  FStdErr.PID := APId;
end;

procedure TExecCmd.RedirectPipes(const ARedirections: TRedirections);
begin
  if (TRedirect.stdin in ARedirections) then
    FStdIn := TPipeStdWriter.Create(STDIN_FILENO)
  else
    FStdIn := TNullStdWriter.Create(STDIN_FILENO);

  if (TRedirect.stdout in ARedirections) then
    FStdOut := TPipeStdReader.Create(STDOUT_FILENO)
  else
    FStdOut := TNullStdReader.Create(STDOUT_FILENO);

  if (TRedirect.stderr in ARedirections) then
    FStdErr := TPipeStdReader.Create(STDERR_FILENO)
  else
    FStdErr := TNullStdReader.Create(STDERR_FILENO);
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

end.
