(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Deploy.Linux64'               *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality:  Make deployables for Linux                            *)
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
unit PyEnvironment.Project.IDE.Deploy.Linux64;

interface

uses
  System.SysUtils,
  PyEnvironment.Project.IDE.Types,
  PyEnvironment.Project.IDE.Deploy.Platform,
  PyEnvironment.Project.IDE.Deploy.Linux;

type
  TPyEnvironmentProjectDeployLinux64 = class(TPyEnvironmentProjectDeployLinux)
  protected
    function GetPlatform: TPyEnvironmentProjectPlatform; override;
    function GetPythonBundleName: string; override;
  end;

implementation

uses
  System.StrUtils;

{ TPyEnvironmentProjectDeployLinux64 }

function TPyEnvironmentProjectDeployLinux64.GetPlatform: TPyEnvironmentProjectPlatform;
begin
    Result := TPyEnvironmentProjectPlatform.Linux64;
end;

function TPyEnvironmentProjectDeployLinux64.GetPythonBundleName: string;
begin
  case IndexStr(GetPythonVersion(), ['3.8', '3.9', '3.10', '3.11', '3.12', '3.13']) of
    0: Result := 'python3-linux-3.8.16-x86_64.zip';
    1: Result := 'python3-linux-3.9.16-x86_64.zip';
    2: Result := 'python3-linux-3.10.11-x86_64.zip';
    3: Result := 'python3-linux-3.11.9-x86_64.zip';
    4: Result := 'python3-linux-3.12.7-x86_64.zip';
    5: Result := 'python3-linux-3.13.0-x86_64.zip';
    else
      Result := String.Empty;
  end;
end;

end.
