unit Requester.Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.Diagnostics, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation, FMX.StdCtrls, PyTools.Notification;

type
  TRequesterForm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

var
  RequesterForm: TRequesterForm;

implementation

uses
  Requested.Form,
  Notification.Content;

{$R *.fmx}

procedure TRequesterForm.Button1Click(Sender: TObject);
begin
  RequestedForm.Show();

  Memo1.Lines.Add('Request sent.');
  Memo1.GoToTextEnd();

  TAppNotificationProtocol.Instance.SendRequest<TBasicResponse>(
    TBasicRequest.Create(Now()),
    procedure(const Arg: TBasicResponse)
    begin
      var LMessage := FormatDateTime('hh:mm:ss', Arg.Body.ResponseTime);
      TThread.Queue(TThread.Current, procedure() begin
        Memo1.Lines.Add('Response time: ' + LMessage);
        Memo1.GoToTextEnd();
      end);
    end,
    procedure(const Arg: TErrorResponseChannel)
    begin
      var LMessage := (Arg as TErrorResponseChannel<TErrorResponseBody>).Body.Message;
      TThread.Queue(TThread.Current, procedure() begin
        Memo1.Lines.Add('Request error: ' + LMessage);
        Memo1.GoToTextEnd();
      end);
    end);
end;

end.
