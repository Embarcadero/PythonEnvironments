unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Platform, PythonEngine,
  PyEnvironment, PyEnvironment.Embeddable;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    Memo1: TMemo;
    PyEmbeddedEnvironment1: TPyEmbeddedEnvironment;
    procedure PyEmbeddedEnvironment1AfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PyEmbeddedEnvironment1AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1BeforeActivate(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1Error(Sender: TObject;
      const AException: Exception);
    procedure PyEmbeddedEnvironment1Ready(Sender: TObject;
      const APythonVersion: string);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FAsyncSetup: IAsyncResult;
    function AppEventHandler(AAppEvent: TApplicationEvent; AContext: TObject): boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

function TForm1.AppEventHandler(AAppEvent: TApplicationEvent;
  AContext: TObject): boolean;
begin
  case AAppEvent of
    TApplicationEvent.FinishedLaunching: begin
      if PyEmbeddedEnvironment1.EndSetup(FAsyncSetup) then begin
        PyEmbeddedEnvironment1.Activate();
      end;
    end;
  end;
  Result := true;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not FAsyncSetup.IsCompleted then
    FAsyncSetup.Cancel();

  FAsyncSetup.AsyncWaitEvent.WaitFor();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAsyncSetup := PyEmbeddedEnvironment1.BeginSetup();
  var LAppEventSErvice := IFMXApplicationEventService(nil);
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(LAppEventService)) then
    LAppEventService.SetApplicationEventHandler(AppEventHandler);

  Memo1.Lines.Add('Background task has started.');
end;

procedure TForm1.PyEmbeddedEnvironment1AfterActivate(Sender: TObject;
  const APythonVersion: string; const AActivated: Boolean);
begin
  TThread.ForceQueue(TThread.Current, procedure() begin
    Memo1.Lines.Add(Format('Python %s has been activated.', [APythonVersion]));
  end);
end;

procedure TForm1.PyEmbeddedEnvironment1AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  TThread.ForceQueue(TThread.Current, procedure() begin
    Memo1.Lines.Add(Format('Python %s has been setup.', [APythonVersion]));
  end);
end;

procedure TForm1.PyEmbeddedEnvironment1BeforeActivate(Sender: TObject;
  const APythonVersion: string);
begin
  TThread.ForceQueue(TThread.Current, procedure() begin
    Memo1.Lines.Add(Format('Activating Python %s.', [APythonVersion]));
  end);
end;

procedure TForm1.PyEmbeddedEnvironment1BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  TThread.ForceQueue(TThread.Current, procedure() begin
    Memo1.Lines.Add(Format('Setting up Python %s.', [APythonVersion]));
  end);
end;

procedure TForm1.PyEmbeddedEnvironment1Error(Sender: TObject;
  const AException: Exception);
begin
  TThread.ForceQueue(TThread.Current, procedure() begin
    Memo1.Lines.Add(Format(
      'An error occurred during the installation process: %s', [
      AException.ToString()]));
  end);
end;

procedure TForm1.PyEmbeddedEnvironment1Ready(Sender: TObject;
  const APythonVersion: string);
begin
  TThread.ForceQueue(TThread.Current, procedure() begin
    Memo1.Lines.Add(Format('Python %s is ready.', [APythonVersion]));
  end);
end;

end.
