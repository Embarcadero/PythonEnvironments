(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Menu'                         *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality: Python project menu creator                            *)
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
unit PyEnvironment.Project.IDE.Menu;

interface

uses
  System.Classes,
  ToolsAPI;

type
  TPyEnvironmentProjectMenuCreatorNotifier = class(TNotifierObject, IOTANotifier, IOTAProjectMenuItemCreatorNotifier)
  strict private
    class var FNotifierIndex: Integer;
    class constructor Create;
    class destructor Destroy;

    { IOTAProjectMenuItemCreatorNotifier }
    procedure AddMenu(const AProject: IOTAProject; const AIdentList: TStrings;
      const AProjectManagerMenuList: IInterfaceList; AIsMultiSelect: Boolean);
  public
    class procedure Register; static;
  end;

  TPyEnvironmenCompileNotifier = class(TInterfacedObject, IOTACompileNotifier)
  strict private
    const
      UnsupportedPlatformMessage =
        'The Python Environment does not support the platform %s in this RAD Studio version.' + sLineBreak + sLineBreak +
        'To avoid problems, disable Python Environment in this project (Project menu > %s) or, if you want to disable it just in ' +
        'a specific platform, set the define directive "%s" in the project settings of this platform. In both cases, ' +
        'be sure you are not using any Python Environment units, otherwise you will get "runtime error" on startup of your application.';
    class var FNotifierIndex: Integer;
    class constructor Create;
    class destructor Destroy;
    { IOTACompileNotifier }
    procedure ProjectCompileStarted(const AProject: IOTAProject; AMode: TOTACompileMode);
    procedure ProjectCompileFinished(const AProject: IOTAProject; AResult: TOTACompileResult);
    procedure ProjectGroupCompileStarted(AMode: TOTACompileMode);
    procedure ProjectGroupCompileFinished(AResult: TOTACompileResult);
  public
    class procedure Register; static;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  Vcl.Dialogs,
  PyEnvironment.Project.IDE.Deploy,
  PyEnvironment.Project.IDE.Helper,
  PyEnvironment.Project.IDE.Types,
  PyEnvironment.Project.IDE.ManagerMenu;

const
  INVALID_MENU_INDEX = -1;

{ TPyEnvironmentProjectMenuCreatorNotifier }

class constructor TPyEnvironmentProjectMenuCreatorNotifier.Create;
begin
  FNotifierIndex := INVALID_MENU_INDEX;
end;

class destructor TPyEnvironmentProjectMenuCreatorNotifier.Destroy;
var
  LProjectManager: IOTAProjectManager;
begin
  if (FNotifierIndex > INVALID_MENU_INDEX)
    and Supports(BorlandIDEServices, IOTAProjectManager, LProjectManager) then
      LProjectManager.RemoveMenuItemCreatorNotifier(FNotifierIndex);
end;

class procedure TPyEnvironmentProjectMenuCreatorNotifier.Register;
var
  LProjectManager: IOTAProjectManager;
begin
  if (FNotifierIndex <= INVALID_MENU_INDEX)
    and Supports(BorlandIDEServices, IOTAProjectManager, LProjectManager) then
      FNotifierIndex := LProjectManager.AddMenuItemCreatorNotifier(
        TPyEnvironmentProjectMenuCreatorNotifier.Create());
end;

procedure TPyEnvironmentProjectMenuCreatorNotifier.AddMenu(
  const AProject: IOTAProject; const AIdentList: TStrings;
  const AProjectManagerMenuList: IInterfaceList; AIsMultiSelect: Boolean);
begin
  if (not AIsMultiSelect)
    and (AIdentList.IndexOf(sProjectContainer) <> -1)
    and Assigned(AProjectManagerMenuList)
    and TPyEnvironmentProjectHelper.SupportsEnvironmentDeployment(AProject) then
  begin
    AProjectManagerMenuList.Add(
      TPyEnvironmentProjectManagerMenuSeparator.Create(pmmpRunNoDebug + 10));
    AProjectManagerMenuList.Add(
      TPyEnvironmentProjectManagerMenuEnablePythonEnvironment.Create(
        AProject, pmmpRunNoDebug + 20));
    AProjectManagerMenuList.Add(
      TPyEnvironmentProjectManagerMenuPythonEnvironmentVersion.Create(
        AProjectManagerMenuList, AProject, pmmpRunNoDebug + 30));
  end;
end;

{ TPyEnvironmenCompileNotifier }

class constructor TPyEnvironmenCompileNotifier.Create;
begin
  FNotifierIndex := INVALID_MENU_INDEX;
end;

class destructor TPyEnvironmenCompileNotifier.Destroy;
var
  LCompileServices: IOTACompileServices;
begin
  if (FNotifierIndex > INVALID_MENU_INDEX)
    and Supports(BorlandIDEServices, IOTACompileServices, LCompileServices) then
      LCompileServices.RemoveNotifier(FNotifierIndex);
end;

procedure TPyEnvironmenCompileNotifier.ProjectCompileFinished(
  const AProject: IOTAProject; AResult: TOTACompileResult);
begin
  //
end;

procedure TPyEnvironmenCompileNotifier.ProjectCompileStarted(
  const AProject: IOTAProject; AMode: TOTACompileMode);
var
  LPlatform: TPyEnvironmentProjectPlatform;
  LConfig: TPyEnvironmentProjectConfig;
  LDeployFile: TPyEnvironmentDeployFile;
  LPythonVersion: string;
begin
  if TPyEnvironmentProjectHelper.SupportsEnvironmentDeployment(AProject) then
  begin
    if Assigned(AProject) then
      LPlatform := TPyEnvironmentProjectPlatform.FromString(AProject.CurrentPlatform)
    else
      LPlatform := TPyEnvironmentProjectPlatform.Unknown;
    if LPlatform = TPyEnvironmentProjectPlatform.Unknown then
      Exit;

    if (AMode in [TOTACompileMode.cmOTAMake, TOTACompileMode.cmOTABuild]) and
      TPyEnvironmentProjectHelper.IsPyEnvironmentDefined[AProject] and TPyEnvironmentProjectDeploy.Found then
    begin
      LPythonVersion := TPyEnvironmentProjectHelper.CurrentPythonVersion[AProject];
      LConfig := TPyEnvironmentProjectConfig.FromString(AProject.CurrentConfiguration);
      if TPyEnvironmentProjectHelper.IsPyEnvironmentDefinedForPlatform(AProject, LPlatform, LConfig) then begin
        if LPlatform in TPyEnvironmentProjectDeploy.SUPPORTED_PLATFORMS then begin
          TPyEnvironmentProjectHelper.RemoveUnexpectedDeployFilesOfClass(
            AProject, LConfig, LPlatform, TPyEnvironmentProjectDeploy.GetDeployFiles(LPythonVersion, LPlatform));
          for LDeployFile in TPyEnvironmentProjectDeploy.GetDeployFiles(LPythonVersion, LPlatform) do begin
            if LDeployFile.CopyToOutput then begin
              Assert(LDeployFile.LocalFileName <> '');
              TPyEnvironmentOTAHelper.TryCopyFileToOutputPathOfActiveBuild(AProject,
                TPath.Combine(TPyEnvironmentProjectDeploy.AbsolutePath, LDeployFile.LocalFileName));
            end;
            TPyEnvironmentProjectHelper.AddDeployFile(AProject, LConfig, LDeployFile);
          end;
        end else begin
          for LDeployFile in TPyEnvironmentProjectDeploy.GetDeployFiles(LPythonVersion, LPlatform) do
            TPyEnvironmentProjectHelper.RemoveDeployFile(
              AProject, LConfig, LPlatform, LDeployFile.LocalFileName, LDeployFile.RemotePath);
          TPyEnvironmentProjectHelper.RemoveDeployFilesOfClass(AProject, LConfig, LPlatform);
          Showmessage(Format(UnsupportedPlatformMessage, [AProject.CurrentPlatform,
            TPyEnvironmentProjectManagerMenuEnablePythonEnvironment.MENU_CAPTIONS[True],
            TPyEnvironmentProjectDeploy.PROJECT_NO_USE_PYTHON]));
        end;
      end else begin
        for LDeployFile in TPyEnvironmentProjectDeploy.GetDeployFiles(LPythonVersion, LPlatform) do
          TPyEnvironmentProjectHelper.RemoveDeployFile(
            AProject, LConfig, LPlatform, LDeployFile.LocalFileName, LDeployFile.RemotePath);
        TPyEnvironmentProjectHelper.RemoveDeployFilesOfClass(AProject, LConfig, LPlatform);
      end;
    end
    {$IF CompilerVersion >= 35}
    else if (AMode = TOTACompileMode.cmOTAClean) and TPyEnvironmentProjectHelper.IsPyEnvironmentDefined[AProject] then begin
      LPythonVersion := TPyEnvironmentProjectHelper.CurrentPythonVersion[AProject];
      for LDeployFile in TPyEnvironmentProjectDeploy.GetDeployFiles(LPythonVersion, LPlatform) do
        if LDeployFile.CopyToOutput then
          TPyEnvironmentOTAHelper.TryRemoveOutputFileOfActiveBuild(AProject, TPath.GetFileName(LDeployFile.LocalFileName));
    end;
    {$ENDIF}
  end;
end;

procedure TPyEnvironmenCompileNotifier.ProjectGroupCompileFinished(
  AResult: TOTACompileResult);
begin

end;

procedure TPyEnvironmenCompileNotifier.ProjectGroupCompileStarted(
  AMode: TOTACompileMode);
begin

end;

class procedure TPyEnvironmenCompileNotifier.Register;
var
  LCompileServices: IOTACompileServices;
begin
  if (FNotifierIndex <= INVALID_MENU_INDEX)
    and Supports(BorlandIDEServices, IOTACompileServices, LCompileServices) then
    FNotifierIndex := LCompileServices.AddNotifier(TPyEnvironmenCompileNotifier.Create);
end;

end.
