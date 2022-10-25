unit Notified.Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, PyTools.Notification;

type
  TNotifiedForm = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMessageEvent: IDisconnectable;
  end;

var
  NotifiedForm: TNotifiedForm;

implementation

uses
  Notification.Content;

{$R *.fmx}

procedure TNotifiedForm.FormCreate(Sender: TObject);
begin
  FMessageEvent := TAppNotificationProtocol.Instance.SubscribeToEvent<TMessageEvent>(
    procedure(const Arg: TMessageEvent) begin
      var LMessage := Arg.Body.MessageContent;
      TThread.Queue(nil, procedure() begin
        Memo1.Lines.Add(LMessage);
        Memo1.GoToTextEnd();
      end);
    end);
end;

procedure TNotifiedForm.FormDestroy(Sender: TObject);
begin
  FMessageEvent.Disconnect();
end;

end.
