(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project'                                  *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality: Provide access to the Python settings selected         *)
(*                 in the project menu                                    *)
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
unit PyEnvironment.Project;

interface

uses
  System.Classes;

type
  TPyEnvironmentProject = class
  private
    function GetPythonVersion: string;
    function GetEnabled: boolean;
  private
    class constructor Create();
    class destructor Destory();
  public
    property Enabled: boolean read GetEnabled;
    property PythonVersion: string read GetPythonVersion;
  end;

var
  PythonProject: TPyEnvironmentProject;

implementation

uses
  System.SysUtils, System.Rtti;

{ TPyEnvironmentProject }

class constructor TPyEnvironmentProject.Create;
begin
  PythonProject := TPyEnvironmentProject.Create();
end;

class destructor TPyEnvironmentProject.Destory;
begin
  FreeAndNil(PythonProject);
end;

function TPyEnvironmentProject.GetEnabled: boolean;
begin
  {$IFDEF PYTHON}
  Result := true;
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

function TPyEnvironmentProject.GetPythonVersion: string;
begin
  {$IFDEF PYTHONVER37}
  Result := '3.7';
  {$ELSEIF DEFINED(PYTHONVER38)}
  Result := '3.8';
  {$ELSEIF DEFINED(PYTHONVER39)}
  Result := '3.9';
  {$ELSEIF DEFINED(PYTHONVER310)}
  Result := '3.10';
  {$ELSE}
  Result := String.Empty;
  {$ENDIF}
end;

end.
