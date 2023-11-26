unit PyTools.ExecCmd.StdIO;

interface

uses
  System.SysUtils,
  System.Rtti,
  {$IFDEF POSIX}
  Posix.Errno,
  {$ELSE !POSIX}
  Winapi.Windows,
  {$ENDIF POSIX}
  PyTools.ExecCmd;

type
  PBytes = ^TBytes;

  TStdBase = class(TInterfacedObject)
  private
    FFileNo: cardinal;
    FPID: integer;
    FEncoding: TEncoding;
  protected
    PipeDescriptors: TPipeDescriptors;
  protected
    function GetPipeDescriptors: TPipeDescriptors;
    function GetPId: integer;
    procedure SetPId(const APId: integer); virtual;
    function GetEncoding: TEncoding;
    procedure SetEncoding(const AEncoding: TEncoding); virtual;
  protected
    function CreateDescriptors(): TPipeDescriptors; virtual; abstract;
  public
    constructor Create(const AFileNo: cardinal); overload; virtual;
    constructor Create(const AFileNo: cardinal; const AEncoding: TEncoding); overload;

    property FileNo: cardinal read FFileNo;
  end;

  TStdReader = class(TStdBase, IStdReader)
  protected
    InternalBuffer: TBytes;
  protected
    function PullMessage(out AByteCount: integer): PBytes; virtual;
    function IsAlive: boolean; virtual; abstract;
  public
    constructor Create(const AFileNo: cardinal); override;

    function ReadNextBytes: TBytes;
    function ReadAllBytes: TBytes; overload;
    function ReadAllBytes(out AValue: TBytes; const ATimeout: cardinal): boolean; overload;

    function ReadNext(const AEncoding: TEncoding): string; overload;
    function ReadNext(): string; overload;
    function ReadAll(const AEncoding: TEncoding): string; overload;
    function ReadAll(): string; overload;
    function ReadAll(out AValue: string; const ATimeout: cardinal; const AEncoding: TEncoding): boolean; overload;
    function ReadAll(out AValue: string; const ATimeout: cardinal): boolean; overload;
  end;

  TStdWriter = class(TStdBase, IStdWriter)
  protected
    procedure PushMessage(const AValue: TBytes); virtual;
  public
    procedure WriteBytes(const AValue: TBytes); overload;

    procedure Write(const AValue: string; const AEncoding: TEncoding); overload;
    procedure Write(const AValue: string); overload;
  end;

const
  BUFFER_SIZE = 1024;

implementation

uses
  System.SyncObjs,
  System.Generics.Collections;

{ TStdBase }

constructor TStdBase.Create(const AFileNo: cardinal);
begin
  inherited Create();
  FFileNo := AFileNo;
  FEncoding := TEncoding.Default;
  PipeDescriptors := CreateDescriptors();
end;

constructor TStdBase.Create(const AFileNo: cardinal; const AEncoding: TEncoding);
begin
  Create(AFileNo);
  FEncoding := AEncoding;
end;

function TStdBase.GetEncoding: TEncoding;
begin
  Result := FEncoding;
end;

function TStdBase.GetPipeDescriptors: TPipeDescriptors;
begin
  Result := PipeDescriptors;
end;

function TStdBase.GetPId: integer;
begin
  Result := FPID;
end;

procedure TStdBase.SetEncoding(const AEncoding: TEncoding);
begin
  FEncoding := AEncoding;
end;

procedure TStdBase.SetPId(const APId: integer);
begin
  FPID := APId;
end;

{ TStdReader }

constructor TStdReader.Create(const AFileNo: cardinal);
begin
  inherited;
  SetLength(InternalBuffer, BUFFER_SIZE);
end;

function TStdReader.PullMessage(out AByteCount: integer): PBytes;
begin
  while True do begin

    {$IFDEF MSWINDOWS}
    if not PeekNamedPipe(PipeDescriptors.ReadDes, nil, 0, nil, @AByteCount, nil) or (AByteCount = 0) then
      Exit(nil);
    {$ENDIF MSWINDOWS}

    AByteCount := FileRead(PipeDescriptors.ReadDes, InternalBuffer[0], BUFFER_SIZE);

    {$IFDEF POSIX}
    if (AByteCount = -1) then begin
      if (errno = EINTR) then
        Continue
      else
        Exit(nil);
    end;
    {$ENDIF POSIX}

    if (AByteCount = 0) then
      Exit(nil)
    else
      Exit(@InternalBuffer);
  end;
end;

function TStdReader.ReadNextBytes: TBytes;
begin
  var LByteCount := 0;
  var LBuff := PullMessage(LByteCount);

  if not Assigned(LBuff) or (LByteCount = 0) then
    Exit(nil);

  SetLength(Result, LByteCount);
  TArray.Copy<Byte>(LBuff^, Result, LByteCount);
end;

function TStdReader.ReadAllBytes(out AValue: TBytes;
  const ATimeout: cardinal): boolean;
begin
  var LValue: TBytes := nil;
  Result := TSpinWait.SpinUntil(
    function(): boolean
    var
      LBuffer: TBytes;
    begin
      LBuffer := ReadNextBytes();
      if Assigned(LBuffer) then
        LValue := LValue + LBuffer;
       //Let's read it until it dies
       Result := not IsAlive();
       //Read until queue is empty, even if it is dead
       if Result then
         Result := not Assigned(LBuffer);
    end, ATimeout);
  AValue := LValue;
end;

function TStdReader.ReadAllBytes: TBytes;
begin
  while not ReadAllBytes(Result, INFINITE) do;
end;

function TStdReader.ReadNext(const AEncoding: TEncoding): string;
begin
  var LByteCount := 0;
  var LBuff := PullMessage(LByteCount);

  if not Assigned(LBuff) or (LByteCount = 0) then
    Exit(String.Empty);

  Result := AEncoding.GetString(LBuff^, Low(LBuff^), LByteCount);
end;

function TStdReader.ReadNext: string;
begin
  Result := ReadNext(GetEncoding());
end;

function TStdReader.ReadAll(out AValue: string;
  const ATimeout: cardinal; const AEncoding: TEncoding): boolean;
var
  LBuff: TBytes;
begin
  Result := ReadAllBytes(LBuff, ATimeout);
  if Assigned(LBuff) then
    AValue := AEncoding.GetString(LBuff, Low(LBuff), Length(LBuff))
  else
    AValue := String.Empty;
end;

function TStdReader.ReadAll(out AValue: string;
  const ATimeout: cardinal): boolean;
begin
  Result := ReadAll(AValue, ATimeout, GetEncoding());
end;

function TStdReader.ReadAll(const AEncoding: TEncoding): string;
begin
  while not ReadAll(Result, INFINITE) do;
end;

function TStdReader.ReadAll: string;
begin
  Result := ReadAll(GetEncoding());
end;

{ TStdWriter }

procedure TStdWriter.PushMessage(const AValue: TBytes);
begin
  FileWrite(PipeDescriptors.WriteDes, AValue[0], Length(AValue));
end;

procedure TStdWriter.WriteBytes(const AValue: TBytes);
begin
  PushMessage(AValue);
end;

procedure TStdWriter.Write(const AValue: string;
  const AEncoding: TEncoding);
begin
  var LBuff := TEncoding.Unicode.Convert(
    TEncoding.Unicode,
    AEncoding,
    TEncoding.Unicode.GetBytes(AValue));

  WriteBytes(LBuff);
end;

procedure TStdWriter.Write(const AValue: string);
begin
  Write(AValue, GetEncoding);
end;

end.
