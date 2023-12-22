(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Editors'                                  *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality: Component editors                                      *)
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
unit PyEnvionment.Editors;

interface

uses
  System.SysUtils, Classes, DesignIntf, DesignEditors;

type
  TPyEnvironmentEmbeddedEditor = class(TComponentEditor)
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
  end;

  TPyEnvironmentEmbeddedPythonVersionProperty = class (TStringProperty)
  public
    procedure SetValue(const Value: string); override;
  end;

procedure Register();

implementation

uses
  ToolsAPI,
  PyEnvironment.Project.IDE.Helper,
  PyEnvironment.Embeddable;

procedure Register();
begin
  RegisterComponentEditor(TPyEmbeddedEnvironment, TPyEnvironmentEmbeddedEditor);
  RegisterPropertyEditor(TypeInfo(string), TPyEmbeddedEnvironment, 'PythonVersion', TPyEnvironmentEmbeddedPythonVersionProperty);
end;

{ TPyEnvironmentEmbeddedPythonVersionProperty }

procedure TPyEnvironmentEmbeddedPythonVersionProperty.SetValue(
  const Value: string);
var
  LProject: IOTAProject;
begin
  LProject := GetActiveProject();
  if Assigned(LProject) and TPyEnvironmentProjectHelper.IsPyEnvironmentDefined[LProject] then begin
    inherited SetValue(TPyEnvironmentProjectHelper.CurrentPythonVersion[LProject])
  end else
    inherited;
end;

{ TPyEnvironmentEmbeddedEditor }

constructor TPyEnvironmentEmbeddedEditor.Create(AComponent: TComponent;
  ADesigner: IDesigner);
var
  LProject: IOTAProject;
  LEnvironment: TPyEmbeddedEnvironment;
  LUpdateNeeded: Boolean;
begin
  inherited;
  LProject := GetActiveProject();
  if TPyEnvironmentProjectHelper.IsPyEnvironmentDefined[LProject] then begin
    LEnvironment := (Self.Component as TPyEmbeddedEnvironment);
    LUpdateNeeded := false;
    if (LEnvironment.PythonVersion <> TPyEnvironmentProjectHelper.CurrentPythonVersion[LProject])
      or LEnvironment.Scanner.AutoScan = false
      or (LEnvironment.Scanner.ScanRule <> TPyEmbeddedEnvironment.TScanRule.srFileName) then
        LUpdateNeeded := true;

    LEnvironment.PythonVersion := TPyEnvironmentProjectHelper.CurrentPythonVersion[LProject];
    LEnvironment.Scanner.AutoScan := true;
    LEnvironment.Scanner.ScanRule := TPyEmbeddedEnvironment.TScanRule.srFileName;

    if LUpdateNeeded then
      Designer.Modified();
  end;
end;

end.
