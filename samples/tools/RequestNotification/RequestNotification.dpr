program RequestNotification;

uses
  System.StartUpCopy,
  FMX.Forms,
  Requester.Form in 'Requester.Form.pas' {RequesterForm},
  Requested.Form in 'Requested.Form.pas' {RequestedForm},
  Notification.Content in 'Notification.Content.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TRequesterForm, RequesterForm);
  Application.CreateForm(TRequestedForm, RequestedForm);
  Application.Run;
end.
