unit PyTools.ExecCmd.StdIO.Pipe;

interface

uses
  System.SysUtils,
  {$IFDEF POSIX}
  Posix.Unistd,
  Posix.SysWait,
  Posix.Errno,
  {$ELSE !POSIX}
  Winapi.Windows,
  {$ENDIF POSIX}
  PyTools.ExecCmd,
  PyTools.ExecCmd.StdIO;

type
  {
    Pipe creates a unidirectional data channel that can be used for
    interprocess communication. The array pipefd is used to return two file
    descriptors referring to the ends of the pipe.

    The Delphi implementation TPipeDescriptors is equivalent to pipefd.
    TPipeDescriptors.ReadDes = pipefd[0] (read end)
    TPipeDescriptors.WriteDes = pipefd[1] (write end)

    Data written to the write end of the pipe is buffered by the kernel until
    it is read from the read end of the pipe.

    ||||||||||||| STDIN |||||||||||||

                pipe[read] (read end) | pipe[write] (write end)
            -------------------------------------------------------------
    Parent  |                                  writes data
            |
    Child   |        reads data
            |

    ||||||||||||| STDOUT AND STDERR |||||||||||||

                pipe[read] (read end) | pipe[write] (write end)
            -------------------------------------------------------------
    Child   |                                  writes data
            |
    Parent  |        reads data
            |

  }

  TPipeStdReader = class(PyTools.ExecCmd.StdIO.TStdReader, IStdReader)
  protected
    procedure SetPId(const APId: integer); override;
  protected
    function CreateDescriptors(): TPipeDescriptors; override;
    function IsAlive: boolean; override;
  public
    destructor Destroy(); override;
  end;

  TPipeStdWriter = class(PyTools.ExecCmd.StdIO.TStdWriter, IStdWriter)
  protected
    procedure SetPId(const APId: integer); override;
  protected
    function CreateDescriptors(): TPipeDescriptors; override;
  public
    destructor Destroy(); override;
  end;

implementation

uses
  PyTools.Exception;

{ TPipeStdReader }

function TPipeStdReader.CreateDescriptors: TPipeDescriptors;
begin
  {$IFDEF POSIX}
  if (pipe(PipeDescriptors) = -1) then
    raise EPipeFailed.Create('Failed to create pipe.');
  {$ELSE !POSIX}
  //Set the bInheritedHandle to true so pipe handles are inherited
  var LSecurityAttributes := Default(TSecurityAttributes);
  with LSecurityAttributes do begin
    nLength := SizeOf(LSecurityAttributes);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;

  if not CreatePipe(PipeDescriptors.ReadDes, PipeDescriptors.WriteDes, @LSecurityAttributes, 0) then
    RaiseLastOSError();
    //Ensure the read handle to the pipe for STDOUT is not inherited
    //if not SetHandleInformation(FStdOutPipeRead, HANDLE_FLAG_INHERIT, 0) then
    //  RaiseLastOSError();
  {$ENDIF POSIX}

  Result := GetPipeDescriptors();
end;

destructor TPipeStdReader.Destroy;
begin
  FileClose(GetPipeDescriptors.ReadDes);
  inherited;
end;

function TPipeStdReader.IsAlive: boolean;
begin
  {$IFDEF POSIX}
  Result := waitpid(GetPID(), nil, WNOHANG or WNOWAIT) = 0;
  {$ELSE !POSIX}
  var LExitCode: cardinal;
  GetExitCodeProcess(GetPID(), LExitCode);
  Result := (LExitCode = STILL_ACTIVE);
  {$ENDIF POSIX}
end;

procedure TPipeStdReader.SetPId(const APId: integer);
begin
  inherited;
  {$IFDEF POSIX}
  if (APId = 0) then begin
    // ---> Here is the child process
    // duplicates stdout/stderr (STDOUT_FILENO|STDERR_FILENO) with the shared data channel (PipeDescriptors).
    // Only the write end is used by the child process.
    while ((dup2(PipeDescriptors.WriteDes, FileNo) = -1) and (errno = EINTR)) do;
    // close unused end of pipe [child process]
    FileClose(PipeDescriptors.ReadDes);
  end else if (APId > 0) then begin
    // ---> Here is the parent process
    // close unused end of pipe [parent process]
    FileClose(PipeDescriptors.WriteDes);
  end;
  {$ELSE !POSIX}
  // Close handles to the stdin and stdout pipes no longer needed by the child process.
  // If they are not explicitly closed, there is no way to recognize that the child process has ended.
  CloseHandle(PipeDescriptors.WriteDes);
  {$ENDIF POSIX}
end;

{ TPipeStdWriter }

destructor TPipeStdWriter.Destroy;
begin
  FileClose(PipeDescriptors.WriteDes);
  inherited;
end;

function TPipeStdWriter.CreateDescriptors: TPipeDescriptors;
begin
  {$IFDEF POSIX}
  if (pipe(PipeDescriptors) = -1) then
    raise EPipeFailed.Create('Failed to create pipe.');
  {$ELSE !POSIX}
  //Set the bInheritedHandle to true so pipe handles are inherited
  var LSecurityAttributes := Default(TSecurityAttributes);
  with LSecurityAttributes do begin
    nLength := SizeOf(LSecurityAttributes);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;

  //Create a pipe for the child process's STDIN
  if not CreatePipe(PipeDescriptors.ReadDes, PipeDescriptors.WriteDes, @LSecurityAttributes, 0) then
    RaiseLastOSError();
  //Ensure the write handle to the pipe for STDIN is not inherited
  //if not SetHandleInformation(FStdInPipeWrite, HANDLE_FLAG_INHERIT, 0) then
  //  RaiseLastOSError();
  {$ENDIF POSIX}

  Result := PipeDescriptors;
end;

procedure TPipeStdWriter.SetPId(const APId: integer);
begin
  inherited;
  {$IFDEF POSIX}
  if (APId = 0) then begin
    // ---> Here is the child process
    // duplicates stdin (STDIN_FILENO) with the shared data channel (FStdInPipe).
    // Only the read end is used by the child process.
    while ((dup2(PipeDescriptors.ReadDes, FileNo) = -1) and (errno = EINTR)) do;
    // close unused end of pipe
    FileClose(PipeDescriptors.WriteDes);
  end else if (APId > 0) then begin
    // ---> Here is the parent process
    // close unused ends of pipe
    FileClose(PipeDescriptors.ReadDes);
  end;
  {$ELSE !POSIX}
  // Close handles to the stdin and stdout pipes no longer needed by the child process.
  // If they are not explicitly closed, there is no way to recognize that the child process has ended.
  CloseHandle(PipeDescriptors.ReadDes);
  {$ENDIF POSIX}
end;

end.
