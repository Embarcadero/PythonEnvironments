unit Requested.Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, PyTools.Notification;

type
  TRequestedForm = class(TForm)
    Memo1: TMemo;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FBasicRequest: IDisconnectable;
    FRequestCount: integer;
  end;

var
  RequestedForm: TRequestedForm;

implementation

uses
  System.SyncObjs,
  Notification.Content;

{$R *.fmx}

procedure TRequestedForm.FormCreate(Sender: TObject);
begin
  FBasicRequest := TAppNotificationProtocol.Instance.SubscribeToRequest<TBasicRequest>(
    function(const Arg: TBasicRequest): TResponseChannel
    begin
      TInterlocked.Increment(FRequestCount);
      TThread.Synchronize(TThread.Current, procedure() begin
        Memo1.Lines.Add(
          'Request received - '
        + FormatDateTime('hh:mm:ss', Arg.Arguments.RequestTime));
        Memo1.GoToTextEnd();
      end);
      if (FRequestCount > 2) and ((FRequestCount mod 2) = 0) then
        Result := TBasicResponseError.Create('This is a response error example.')
      else
        Result := TBasicResponse.Create(Now());
    end);
end;

procedure TRequestedForm.FormDestroy(Sender: TObject);
begin
  FBasicRequest.Disconnect();
end;

end.
