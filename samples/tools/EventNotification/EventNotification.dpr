program EventNotification;

uses
  System.StartUpCopy,
  FMX.Forms,
  Notifier.Form in 'Notifier.Form.pas' {NotifierForm},
  Notified.Form in 'Notified.Form.pas' {NotifiedForm},
  Notification.Content in 'Notification.Content.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TNotifierForm, NotifierForm);
  Application.CreateForm(TNotifiedForm, NotifiedForm);
  Application.Run;
end.
