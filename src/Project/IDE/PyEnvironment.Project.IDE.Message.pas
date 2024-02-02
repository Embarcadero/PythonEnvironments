(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Message'                      *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality: Helpers for the Python project menu                    *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)
unit PyEnvironment.Project.IDE.Message;

interface

uses
  System.SysUtils,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  ToolsAPI;

type
  TPythonMessage = class
  private
    class function GetMessageGroup: IOTAMessageGroup; static;
    class procedure MakeMessageWindowBigger();
  public
    class function GetMessageViewWindow(): TWinControl;
    class function GetMessageViewControl(): TWinControl;
  public
    class procedure Clear(); static;
    class procedure ShowMessageView(const AMakeMessageWindowBigger: boolean = false); static;

    class function ToolMessage(const AStatus, AMessage, APrefix: string; const AParent: pointer = nil): pointer; overload; static;
    class function CustomMessage(const AMessage: IOTACustomMessage; const AParent: pointer = nil): pointer; overload; static;

    class procedure BuildingFiles(const APythonVersion: string);
    class procedure CleaningFiles(const APythonVersion: string);

    class procedure WarnControlsLocked();
    class procedure AllDone();
  end;

  TCustomMessage = class(TInterfacedObject, IOTACustomMessage)
  private
    FText: string;
  protected
    procedure SetText(const Value: string); virtual;
    function GetText(): string; virtual;
  private
    function GetColumnNumber: Integer;
    /// <summary>
    /// Returns a Fully qualified filename if this message line can navigate to a
    /// file line
    /// </summary>
    function GetFileName: string;
    /// <summary>
    /// Returns the Line number of the above file if the above file is given
    /// </summary>
    function GetLineNumber: Integer;
    /// <summary>
    /// Returns the Raw line text
    /// </summary>
    function GetLineText: string;
    /// <summary>
    /// F1 pressed on this line
    /// </summary>
    procedure ShowHelp;
  public
    constructor Create();
    destructor Destroy(); override;

    property Text: string read FText write SetText;
  end;

  TTextMessage = class(TCustomMessage)
  public
    constructor Create(const AText: string);
  end;

  TCustomProgressBarMessage = class(TCustomMessage, INTACustomDrawMessage)
  private
    FPercentage: integer;
    FPrintPercentage: boolean;
    function GetParent: TWinControl;
    procedure SetPercentage(const Value: integer);
  protected
    const LINE_HEIGHT = 30;
    const OFFSET = 30;
  protected
    Canvas: TCanvas;
    OriginalRect: TRect;
  protected
    // INTACustomDrawMessage implementation
    procedure Draw(Canvas: TCanvas; const Rect: TRect; Wrap: Boolean); virtual;
    function CalcRect(Canvas: TCanvas; MaxWidth: Integer; Wrap: Boolean): TRect; virtual;
  protected
    procedure SetText(const Value: string); override;
    function GetProgressBarSize: TSize; virtual;
    procedure Invalidate(); virtual;
    procedure Print(); virtual;
    procedure Update();
  public
    constructor Create();

    procedure Paint();

    property Percentage: integer read FPercentage write SetPercentage;
    property PrintPercentage: boolean read FPrintPercentage write FPrintPercentage;
  end;

  TTextProgressBarMessage = class(TCustomProgressBarMessage)
  protected
    function GetText(): string; override;
    procedure Print(); override;
  end;

const
  PYTHON_GROUP = 'Python';

implementation

uses
  System.Classes,
  System.IOUtils,
  System.Math,
  Vcl.Themes,
  Vcl.Forms,
  WinApi.Windows;

{ TPythonMessage }

class function TPythonMessage.GetMessageGroup: IOTAMessageGroup;
var
  LMessageServices: IOTAMessageServices;
begin
  LMessageServices := (BorlandIDEServices as IOTAMessageServices);
  Result := LMessageServices.GetGroup(PYTHON_GROUP);
  if not Assigned(Result) then begin
    Result := LMessageServices.AddMessageGroup(PYTHON_GROUP);
    Result.AutoScroll := true;
  end;
end;

class function TPythonMessage.GetMessageViewWindow: TWinControl;
begin
  for var I := 0 to Screen.FormCount - 1 do
    if (Screen.Forms[I].Name = 'MessageViewForm') then
      Exit(Screen.Forms[I]);

  Result := nil;
end;

class function TPythonMessage.GetMessageViewControl: TWinControl;
var
  LWindow: TWinControl;
begin
  LWindow := GetMessageViewWindow();
  if not Assigned(LWindow) then
    Exit(nil);

  Result := LWindow.FindComponent('MessageTreeView1') as TWinControl;
end;

class procedure TPythonMessage.MakeMessageWindowBigger;
const
  MAX_HEIGHT = 800;
var
  LWindow: TWinControl;
  LDockPanel: TWinControl;
begin
  LWindow := GetMessageViewWindow();
  if not Assigned(LWindow) then
    Exit;

  LDockPanel := LWindow.Parent;
  if not Assigned(LDockPanel) then
    Exit;

  LDockPanel.Height := Max(
    LDockPanel.Height,
    Min(MAX_HEIGHT, (Application.MainForm.Height * 80) div 100));
end;

class procedure TPythonMessage.ShowMessageView(
  const AMakeMessageWindowBigger: boolean);
var
  LMessageServices: IOTAMessageServices;
begin
  LMessageServices := (BorlandIDEServices as IOTAMessageServices);
  LMessageServices.ShowMessageView(TPythonMessage.GetMessageGroup());

  if AMakeMessageWindowBigger then
    TPythonMessage.MakeMessageWindowBigger();
end;

class procedure TPythonMessage.WarnControlsLocked;
var
  LMessageServices: IOTAMessageServices;
begin
  LMessageServices := (BorlandIDEServices as IOTAMessageServices);
  LMessageServices.AddCustomMessage(
    TTextMessage.Create(
    'Warning: The IDE controls will be locked until all operations have been completed.'),
    TPythonMessage.GetMessageGroup());
end;

class procedure TPythonMessage.AllDone;
var
  LMessageServices: IOTAMessageServices;
begin
  LMessageServices := (BorlandIDEServices as IOTAMessageServices);
  LMessageServices.AddTitleMessage('All done.', TPythonMessage.GetMessageGroup());
end;

class procedure TPythonMessage.BuildingFiles(const APythonVersion: string);
var
  LMessageServices: IOTAMessageServices;
begin
  LMessageServices := (BorlandIDEServices as IOTAMessageServices);
  LMessageServices.AddTitleMessage(
    Format('>>> Building Python %s <<<', [APythonVersion]),
    TPythonMessage.GetMessageGroup());
end;

class procedure TPythonMessage.CleaningFiles(const APythonVersion: string);
var
  LMessageServices: IOTAMessageServices;
begin
  LMessageServices := (BorlandIDEServices as IOTAMessageServices);
  LMessageServices.AddTitleMessage(
    Format('>>> Cleaning Python %s <<<', [APythonVersion]),
    TPythonMessage.GetMessageGroup());
end;

class procedure TPythonMessage.Clear;
var
  LMessageServices: IOTAMessageServices;
begin
  LMessageServices := (BorlandIDEServices as IOTAMessageServices);
  LMessageServices.ClearMessageGroup(TPythonMessage.GetMessageGroup());
end;

class function TPythonMessage.ToolMessage(const AStatus, AMessage, APrefix: string;
  const AParent: pointer): pointer;
var
  LMessageServices: IOTAMessageServices;
begin
  LMessageServices := (BorlandIDEServices as IOTAMessageServices);

  LMessageServices.AddToolMessage(
    String.Empty, AStatus + ': ' + AMessage, APrefix, -1, -1,
    AParent, Result, TPythonMessage.GetMessageGroup());
end;

class function TPythonMessage.CustomMessage(const AMessage: IOTACustomMessage;
  const AParent: pointer): pointer;
var
  LMessageServices: IOTAMessageServices;
begin
  LMessageServices := (BorlandIDEServices as IOTAMessageServices);

  if not Assigned(AParent) then
    Result := LMessageServices.AddCustomMessagePtr(AMessage, TPythonMessage.GetMessageGroup())
  else
    Result := LMessageServices.AddCustomMessage(AMessage, AParent);
end;

{ TCustomMessage }

constructor TCustomMessage.Create;
begin
  inherited;
end;

destructor TCustomMessage.Destroy;
begin
  inherited;
end;

function TCustomMessage.GetText: string;
begin
  Result := FText;
end;

procedure TCustomMessage.SetText(const Value: string);
begin
  FText := Value;
end;

function TCustomMessage.GetColumnNumber: Integer;
begin
  Result := -1;
end;

function TCustomMessage.GetFileName: string;
begin
  Result := String.Empty;
end;

function TCustomMessage.GetLineNumber: Integer;
begin
  Result := -1;
end;

function TCustomMessage.GetLineText: string;
begin
  Result := FText;
end;

procedure TCustomMessage.ShowHelp;
begin
  //
end;

{ TTextMessage }

constructor TTextMessage.Create(const AText: string);
begin
  inherited Create();
  Text := AText;
end;

{ TCustomProgressBarMessage }

constructor TCustomProgressBarMessage.Create;
begin
  inherited;
  FPrintPercentage := true;
end;

procedure TCustomProgressBarMessage.SetPercentage(const Value: integer);
begin
  if FPercentage = Value then
    Exit;

  FPercentage := Value;
  // Repaint parent to invalidate this message
  Update();
end;

procedure TCustomProgressBarMessage.SetText(const Value: string);
begin
  inherited;
  Update();
end;

function TCustomProgressBarMessage.CalcRect(Canvas: TCanvas; MaxWidth: Integer;
  Wrap: Boolean): TRect;
begin
  Result := Default(TRect);
end;

procedure TCustomProgressBarMessage.Draw(Canvas: TCanvas; const Rect: TRect;
  Wrap: Boolean);
begin
  Self.Canvas := Canvas;
  Self.OriginalRect := Rect;
  Paint();
end;

function TCustomProgressBarMessage.GetParent: TWinControl;
begin
  Result := TPythonMessage.GetMessageViewControl();

  if not Assigned(Result) then
    Result := Application.MainForm;
end;

function TCustomProgressBarMessage.GetProgressBarSize: TSize;
begin
  Result := TSize.Create(Canvas.TextWidth(GetText()), LINE_HEIGHT);
end;

procedure TCustomProgressBarMessage.Invalidate;
begin
  Canvas.FillRect(
    TRect.Create(OFFSET, 0, OriginalRect.Width, GetProgressBarSize().Height));
end;

procedure TCustomProgressBarMessage.Paint;
begin
  Invalidate();
  Print();
end;

procedure TCustomProgressBarMessage.Update;
var
  LParent: TWinControl;
begin
  LParent := GetParent();
  if Assigned(LParent) then
    LParent.Invalidate(); //This will trigger "Draw"
end;

procedure TCustomProgressBarMessage.Print;
var
  LBrushColor: TColor;
  LBrushStyle: TBrushStyle;
  LSize: TSize;
begin
  LBrushColor := Canvas.Brush.Color;
  LBrushStyle := Canvas.Brush.Style;
  try
    Canvas.Brush.Style := TBrushStyle.bsSolid;
    Canvas.Brush.Color := clLtGray;
    // Fill up background in percentage
    LSize := GetProgressBarSize();
    Canvas.FillRect(
      TRect.Create(
        OFFSET, 0, ((FPercentage * LSize.Width) div 100) + OFFSET, LSize.Height));
  finally
    Canvas.Brush.Style := LBrushStyle;
    Canvas.Brush.Color := LBrushColor;
  end;
end;

{ TTextProgressBarMessage }

function TTextProgressBarMessage.GetText: string;
begin
  if PrintPercentage then
    Result := FText + ' (' + Percentage.ToString() + '%)'
  else
    Result := FText;
end;

procedure TTextProgressBarMessage.Print;
var
  LBrushStyle: TBrushStyle;
  LSize: TSize;
begin
  inherited;
  LBrushStyle := Canvas.Brush.Style;
  try
    Canvas.Brush.Style := TBrushStyle.bsClear;
    LSize := GetProgressBarSize();
    Canvas.TextRect(
      TRect.Create(0, 0, LSize.Width + OFFSET, LSize.Height),
      OFFSET, 0, GetText());
  finally
    Canvas.Brush.Style := LBrushStyle;
  end;
end;

end.
