unit PyTools.Exception;

interface

uses
  System.SysUtils;

type
  EChannelTypeAttributeNotFound = class(Exception);

  EExecCmd = class(Exception);

  EForkFailed = class(EExecCmd);

  EPipeFailed = class(EExecCmd);

  EInvalidArgument = class(EExecCmd);

  EOperationNotPermitted = class(EExecCmd);

  ENoSuchProcess = class(EExecCmd);

  EWaitFailed = class(EExecCmd);

  EExecCmdFailed = class(EExecCmd)
  private
    FCode: integer;
  public
    constructor Create(const AMessage: string; const ACode: integer); reintroduce;

    property Code: integer read FCode;
  end;

implementation

{ EExecCmdFailed }

constructor EExecCmdFailed.Create(const AMessage: string; const ACode: integer);
begin
  inherited Create(AMessage);
  FCode := ACode;
end;

end.
