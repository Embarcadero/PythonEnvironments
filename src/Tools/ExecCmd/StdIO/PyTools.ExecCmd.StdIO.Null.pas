unit PyTools.ExecCmd.StdIO.Null;

interface

uses
  System.SysUtils,
  {$IFDEF POSIX}
  //
  {$ELSE !POSIX}
  Winapi.Windows,
  {$ENDIF POSIX}
  PyTools.ExecCmd,
  PyTools.ExecCmd.StdIO;

type
  TNullStdReader = class(PyTools.ExecCmd.StdIO.TStdReader, IStdReader)
  protected
    function CreateDescriptors(): TPipeDescriptors; override;
    function PullMessage(out AByteCount: integer): PBytes; override;
    function IsAlive: boolean; override;
  end;

  TNullStdWriter = class(PyTools.ExecCmd.StdIO.TStdWriter, IStdWriter)
  protected
    function CreateDescriptors(): TPipeDescriptors; override;
    procedure PushMessage(const AValue: TBytes); override;
  end;

implementation

{ TNullStdReader }

function TNullStdReader.CreateDescriptors: TPipeDescriptors;
begin
  {$IFDEF POSIX}
  Result.ReadDes := FileNo;
  Result.WriteDes := -1;
  {$ELSE !POSIX}
  Result.ReadDes := GetStdHandle(FileNo);
  Result.WriteDes := 0;
  {$ENDIF POSIX}
end;

function TNullStdReader.IsAlive: boolean;
begin
  Result := false;
end;

function TNullStdReader.PullMessage(out AByteCount: integer): PBytes;
begin
  Result := nil;
end;

{ TNullStdWriter }

function TNullStdWriter.CreateDescriptors: TPipeDescriptors;
begin
  {$IFDEF POSIX}
  Result.ReadDes := -1;
  Result.WriteDes := FileNo;
  {$ELSE !POSIX}
  Result.ReadDes := 0;
  Result.WriteDes := GetStdHandle(FileNo);
  {$ENDIF POSIX}
end;

procedure TNullStdWriter.PushMessage(const AValue: TBytes);
begin
  //
end;

end.
