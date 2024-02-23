(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Deploy.Factory'               *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality:  Create deployment instances of a given platform       *)
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
unit PyEnvironment.Project.IDE.Deploy.Factory;

interface

uses
  PyEnvironment.Project.IDE.Types,
  PyEnvironment.Project.IDE.Deploy.Intf;

type
  TPyEnvironmentProjectDeploySimpleFactory = class
  public
    class function CreateInstance(const AModel: TDeployFilesModel): IDeploymentTask; static;
  end;

implementation

uses
  PyEnvironment.Project.IDE.Deploy.Platform,
  PyEnvironment.Project.IDE.Deploy.Windows32,
  PyEnvironment.Project.IDE.Deploy.Windows64,
  PyEnvironment.Project.IDE.Deploy.AndroidARM,
  PyEnvironment.Project.IDE.Deploy.AndroidARM64,
  PyEnvironment.Project.IDE.Deploy.OSX64,
  PyEnvironment.Project.IDE.Deploy.OSXARM64,
  PyEnvironment.Project.IDE.Deploy.iOSSimARM64,
  PyEnvironment.Project.IDE.Deploy.iOSDevice64,
  PyEnvironment.Project.IDE.Deploy.Linux64;

{ TPyEnvironmentProjectDeploySimpleFactory }

class function TPyEnvironmentProjectDeploySimpleFactory.CreateInstance(
  const AModel: TDeployFilesModel): IDeploymentTask;
var
  LPlatformClass: TPyEnvironmentProjectDeployPlatformClass;
begin
  LPlatformClass := nil;
  case AModel.Platform of
    Win32: LPlatformClass := TPyEnvironmentProjectDeployWindows32;
    Win64: LPlatformClass := TPyEnvironmentProjectDeployWindows64;
    Android: LPlatformClass := TPyEnvironmentProjectDeployAndroidARM;
    Android64: LPlatformClass := TPyEnvironmentProjectDeployAndroidARM64;
    iOSDevice64: LPlatformClass := TPyEnvironmentProjectDeployIOSDevice64;
    iOSSimARM64: LPlatformClass := TPyEnvironmentProjectDeployIOSSimARM64;
    OSX64: LPlatformClass := TPyEnvironmentProjectDeployOSX64;
    OSXARM64: LPlatformClass := TPyEnvironmentProjectDeployOSXARM64;
    Linux64: LPlatformClass := TPyEnvironmentProjectDeployLinux64;
  end;

  if not Assigned(LPlatformClass) then
    Exit(nil);

  Result := LPlatformClass.Create(AModel);
end;

end.
