unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  PythonEngine, PyEnvironment, PyEnvironment.Embeddable,
  FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Objects, FMX.TabControl;

type
  TForm1 = class(TForm)
    PyEmbeddedEnvironment1: TPyEmbeddedEnvironment;
    PythonEngine1: TPythonEngine;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Image2: TImage;
    TabItem3: TTabItem;
    Memo1: TMemo;
    Image3: TImage;
    procedure PyEmbeddedEnvironment1PluginInstall(const APlugin: TObject;
      const AInfo: TPyPluginInfo);
    procedure PyEmbeddedEnvironment1BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1BeforeActivate(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1AfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PyEmbeddedEnvironment1Error(Sender: TObject;
      const AException: Exception);
    procedure PyEmbeddedEnvironment1Ready(Sender: TObject;
      const APythonVersion: string);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

// Set the default view tab to "Run"
procedure TForm1.FormShow(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem3;
end;

// "AfterActivate" Event of PyEmbeddedEnvironment1 is assigned
// to the below procedure
procedure TForm1.PyEmbeddedEnvironment1AfterActivate(Sender: TObject;
  const APythonVersion: string; const AActivated: Boolean);
begin
  Memo1.Lines.Add(Format('Python %s is active.', [APythonVersion]));
end;

// "AfterSetUp" Event of PyEmbeddedEnvironment1 is assigned
// to the below procedure
procedure TForm1.PyEmbeddedEnvironment1AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Python %s setup done.', [APythonVersion]));
end;

// "BeforeActivate" Event of PyEmbeddedEnvironment1 is assigned
// to the below procedure
procedure TForm1.PyEmbeddedEnvironment1BeforeActivate(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Activating Python %s.', [APythonVersion]));
end;

// "BeforeSetUp" Event of PyEmbeddedEnvironment1 is assigned
// to the below procedure
procedure TForm1.PyEmbeddedEnvironment1BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Setting up Python %s.', [APythonVersion]));
end;

// "OnError" Event of PyEmbeddedEnvironment1 is assigned
// to the below procedure
procedure TForm1.PyEmbeddedEnvironment1Error(Sender: TObject;
  const AException: Exception);
begin
  Memo1.Lines.Add(Format('An error has occurred: %s', [AException.Message]));
end;

// "OnPluginInstall" Event of PyEmbeddedEnvironment1 is assigned
// to the below procedure
procedure TForm1.PyEmbeddedEnvironment1PluginInstall(const APlugin: TObject;
  const AInfo: TPyPluginInfo);
begin
  Memo1.Lines.Add(Format('Installing %s...', [AInfo.Name]));
end;

// "OnReady" Event of PyEmbeddedEnvironment1 is assigned
// to the below procedure
procedure TForm1.PyEmbeddedEnvironment1Ready(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('The Python %s environment is ready.', [APythonVersion]));
end;

end.
