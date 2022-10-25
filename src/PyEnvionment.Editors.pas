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
  TPyEnvironmentEmbeddedPythonVersionProperty = class (TStringProperty)
  public
    function GetValue: string; override;
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
  RegisterPropertyEditor(TypeInfo(string), TPyEmbeddedEnvironment, 'PythonVersion', TPyEnvironmentEmbeddedPythonVersionProperty);
end;

{ TPyEnvironmentEmbeddedPythonVersionProperty }

function TPyEnvironmentEmbeddedPythonVersionProperty.GetValue: string;
var
  LProject: IOTAProject;
begin
  LProject := GetActiveProject();
  if Assigned(LProject) then begin
    if TPyEnvironmentProjectHelper.IsPyEnvironmentDefined[LProject] then
      Result := TPyEnvironmentProjectHelper.CurrentPythonVersion[LProject]
    else
      Result := inherited;
  end else
    Result := inherited;
end;

procedure TPyEnvironmentEmbeddedPythonVersionProperty.SetValue(
  const Value: string);
var
  LProject: IOTAProject;
begin
  LProject := GetActiveProject();
  if Assigned(LProject) then begin
    if not TPyEnvironmentProjectHelper.IsPyEnvironmentDefined[LProject] then
      inherited;
  end else
    inherited;
end;

end.
