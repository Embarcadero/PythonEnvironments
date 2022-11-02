unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, PythonEngine,
  PyEnvironment, PyEnvironment.Embeddable, FMX.StdCtrls, System.Zip;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    Memo1: TMemo;
    PyEmbeddedEnvironment1: TPyEmbeddedEnvironment;
    StatusBar1: TStatusBar;
    Label1: TLabel;
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
    procedure PyEmbeddedEnvironment1ZipProgress(Sender: TObject;
      ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
      Header: TZipHeader; Position: Int64);
  private
    FAsyncActivate: IAsyncResult;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils;

{$R *.fmx}

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FAsyncActivate.IsCompleted;
  if not CanClose then
    ShowMessage('Waiting for background operation. Try again.');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  LAsyncSetup: IAsyncResult;
begin
  LAsyncSetup := PyEmbeddedEnvironment1.SetupAsync();
  FAsyncActivate := PyEmbeddedEnvironment1.ActivateAsync(LAsyncSetup);
  Memo1.Lines.Add('Background task has started.');
end;

procedure TForm1.PyEmbeddedEnvironment1AfterActivate(Sender: TObject;
  const APythonVersion: string; const AActivated: Boolean);
begin
  Memo1.Lines.Add(Format('Python %s has been activated.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedEnvironment1AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Python %s has been setup.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedEnvironment1BeforeActivate(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Activating Python %s.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedEnvironment1BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Setting up Python %s.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedEnvironment1Error(Sender: TObject;
  const AException: Exception);
begin
  Memo1.Lines.Add(Format(
    'An error occurred during the installation process: %s', [
    AException.ToString()]));
end;

procedure TForm1.PyEmbeddedEnvironment1Ready(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Python %s is ready.', [APythonVersion]));
end;

// Assigning the below procedure to the "OnZipProgress" event of
// TPyEmbeddedEnvironment1 component
procedure TForm1.PyEmbeddedEnvironment1ZipProgress(Sender: TObject;
  ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  //Zip progress is near synchronized, even when
  //the SynchronizeEvents property is set to true
  TThread.Queue(nil, procedure() begin
    Label1.Text := FileName.Replace(
      TDirectory.GetParent(ADistribution.EnvironmentPath), String.Empty, []);
  end)
end;

end.
