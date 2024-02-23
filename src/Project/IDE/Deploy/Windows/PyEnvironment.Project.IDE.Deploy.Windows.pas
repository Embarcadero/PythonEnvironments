(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Deploy.Windows'               *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality:  Make deployables for Android                          *)
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
unit PyEnvironment.Project.IDE.Deploy.Windows;

interface

uses
  System.SysUtils,
  System.Classes,
  ToolsAPI,
  DeploymentAPI,
  PyEnvironment.Project.IDE.Types,
  PyEnvironment.Project.IDE.Deploy.Platform;

type
  TPyEnvironmentProjectDeployWindows = class(TPyEnvironmentProjectDeployPlatform)
  protected
    function Make(const AInput: TDeployTaskInput): TDeployTaskOutput; override;
    function Deploy(const AInput: TDeployTaskInput): TDeployTaskOutput; override;
  end;

implementation

uses
  System.IOUtils,
  PyEnvironment.Project.IDE.Deploy;

{ TPyEnvironmentProjectDeployWindows }

function TPyEnvironmentProjectDeployWindows.Make(
  const AInput: TDeployTaskInput): TDeployTaskOutput;
var
  LFileName: string;
begin
  // Create a minimal Python bundle
  LFileName := GetBundleMinimalFileName();

  // Always rebuild ??? Not now...
  if TFile.Exists(LFileName) then
    Exit(true);

  if not TDirectory.Exists(TPath.GetDirectoryName(LFileName)) then
    TDirectory.CreateDirectory(TPath.GetDirectoryName(LFileName));

  // Create the minimal distribution for windows - it is only a copy for now
  TFile.Copy(LocatePythonBundle(), LFileName);

  Result := TFile.Exists(LFileName);
end;

function TPyEnvironmentProjectDeployWindows.Deploy(
  const AInput: TDeployTaskInput): TDeployTaskOutput;
var
  LFileName: string;
  LFiles: TPyEnvironmentDeployFiles;
begin
  LFileName := GetBundleMinimalFileName();
  if not TFile.Exists(LFileName) then
    Exit(false);

  // Add the python minimal bundle to the deploy file list
  LFiles := [
    TPyEnvironmentDeployFile.Create(GetPlatform(),
      LFileName.Replace(IncludeTrailingPathDelimiter(GetEnvironmentFolder()), '', []),
      '.\',
      true,  true, TDeployOperation.doCopyOnly, '')
    ];

  Result := Assigned(LFiles);
  Result.Args.SetFiles(LFiles);
end;

end.
