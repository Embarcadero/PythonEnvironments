unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, PythonEngine,
  PyEnvironment, PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python39;

type
  TForm1 = class(TForm)
    PyEmbeddedResEnvironment391: TPyEmbeddedResEnvironment39;
    PythonEngine1: TPythonEngine;
    Memo1: TMemo;
    procedure PyEmbeddedResEnvironment391Error(Sender: TObject;
      const AException: Exception);
    procedure PyEmbeddedResEnvironment391AfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PyEmbeddedResEnvironment391AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment391BeforeActivate(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment391BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment391Ready(Sender: TObject;
      const APythonVersion: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.PyEmbeddedResEnvironment391AfterActivate(Sender: TObject;
  const APythonVersion: string; const AActivated: Boolean);
begin
  Memo1.Lines.Add(Format('Python %s has been activated.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedResEnvironment391AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Python %s has been setup.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedResEnvironment391BeforeActivate(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Activating Python %s.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedResEnvironment391BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Setting up Python %s.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedResEnvironment391Error(Sender: TObject;
  const AException: Exception);
begin
  Memo1.Lines.Add(Format(
    'An error occurred during the installation process: %s', [
    AException.ToString()]));
end;

procedure TForm1.PyEmbeddedResEnvironment391Ready(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Python %s is ready.', [APythonVersion]));
end;

end.
