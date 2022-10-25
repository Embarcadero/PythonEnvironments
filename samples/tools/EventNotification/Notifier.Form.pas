unit Notifier.Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.Diagnostics, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation, FMX.StdCtrls, PyTools.Notification;

type
  TNotifierForm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

var
  NotifierForm: TNotifierForm;

implementation

uses
  Notified.Form,
  Notification.Content;

{$R *.fmx}

procedure TNotifierForm.Button1Click(Sender: TObject);
begin
  var LDateTime := Now();
  Memo1.Lines.Add(
    FormatDateTime('hh:mm:ss', LDateTime)
  + ' - Event broadcasted.');
  Memo1.GoToTextEnd();

  NotifiedForm.Show();
  TAppNotificationProtocol.Instance.BroadcastEvent(
    TMessageEvent.Create(
      FormatDateTime('hh:mm:ss', LDateTime)
    + ' - Message sent from '
    + Self.ClassName
    + '.'));
end;

end.
