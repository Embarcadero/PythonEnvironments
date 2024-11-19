(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Deploy'                       *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality: Deploy Python embeddables using project menu           *)
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
unit PyEnvironment.Project.IDE.Deploy;

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Threading,
  System.Generics.Collections,
  ToolsAPI,
  DeploymentAPI,
  PyEnvironment.Project.IDE.Types;

type
  TPyEnvironmentProjectDeploy = class
  strict private type
    TFilesReadyCallback = TProc<TPyEnvironmentProjectPlatform, TArray<TPyEnvironmentDeployFile>>;
    TExceptionCallback = TProc<TPyEnvironmentProjectPlatform, Exception>;
    TTerminatedCallback = TProc;
  strict private
    class var
      FAbsolutePath: string;
      FPath: string;
      FPathChecked: Boolean;
      FLock: TObject;
    class procedure FindPath(out APath, AAbsolutePath: string); static;
    class function GetAbsolutePath: string; static;
    class function GetFound: Boolean; static;
    class function GetPath: string; static;
    class function IsValidPythonEnvironmentDir(const APath: string): Boolean; static;

    class procedure GetPlatformDeployFiles(
      const AModel: TDeployFilesModel;
      // OnFilesReady
      const AFilesReadyCallback: TFilesReadyCallback;
      // OnException
      const AExceptionCallback: TExceptionCallback);
    class function GetPlatformDeployFilesAsync(
      const AModel: TDeployFilesModel;
      // OnFilesReady
      const AFilesReadyCallback: TFilesReadyCallback;
      // OnException
      const AExceptionCallback: TExceptionCallback): ITask;

    class procedure BeginExecute(const APythonVersion: string;
      const ACleaning: boolean); static;
    class procedure EndExecute(); static;
  public
    const
      DEPLOYMENT_CLASS = 'Python';
      PROJECT_USE_PYTHON = 'PYTHON';
      PROJECT_NO_USE_PYTHON = 'NOPYTHON';
      PYTHON_ENVIRONMENT_DIR_VARIABLE = 'PYTHONENVIRONMENTDIR';
      PYTHON_VERSIONS: array[0..5] of string = ('3.8', '3.9', '3.10', '3.11', '3.12', '3.13');
      SUPPORTED_PLATFORMS = [
        TPyEnvironmentProjectPlatform.Win32, TPyEnvironmentProjectPlatform.Win64,
        TPyEnvironmentProjectPlatform.Android, TPyEnvironmentProjectPlatform.Android64,
        TPyEnvironmentProjectPlatform.iOSSimARM64, TPyEnvironmentProjectPlatform.iOSDevice64,
        TPyEnvironmentProjectPlatform.OSX64, TPyEnvironmentProjectPlatform.OSXARM64,
        TPyEnvironmentProjectPlatform.Linux64];
  public
    class constructor Create();
    class destructor Destroy();

    class procedure GetDeployFiles(
      const AProjectName, APythonVersion: string;
      const APlatforms: TPyEnvironmentProjectPlatforms;
      const ACleaning: boolean;
      // OnFilesReady
      const AFilesReadyCallback: TFilesReadyCallback;
      // OnException
      const AExceptionCallback: TExceptionCallback;
      // OnTerminate
      const ATerminatedCallback: TTerminatedCallback); overload; static;

    class procedure GetDeployFilesAsync(
      const AProjectName, APythonVersion: string;
      const APlatforms: TPyEnvironmentProjectPlatforms;
      const ACleaning: boolean;
      // OnFilesReady
      const AFilesReadyCallback: TFilesReadyCallback;
      // OnException
      const AExceptionCallback: TExceptionCallback;
      // OnTerminate
      const ATerminatedCallback: TTerminatedCallback);

    class function GetDeployFiles(const AProjectName, APythonVersion: string;
      const APlatform: TPyEnvironmentProjectPlatform; const ACleaning: boolean)
    : TArray<TPyEnvironmentDeployFile>; overload; static;

    class property AbsolutePath: string read GetAbsolutePath;
    class property Found: Boolean read GetFound;
    class property Path: string read GetPath;
  end;

implementation

uses
  System.IOUtils,
  System.StrUtils,
  Vcl.Forms,
  PyEnvironment.Project.IDE.Helper,
  PyEnvironment.Project.IDE.Deploy.Intf,
  PyEnvironment.Project.IDE.Deploy.Factory,
  PyEnvironment.Project.IDE.Deploy.Manager, PyEnvironment.Project.IDE.Message;

{ TPyEnvironmentProject }

class constructor TPyEnvironmentProjectDeploy.Create;
begin
  FLock := TObject.Create();
end;

class destructor TPyEnvironmentProjectDeploy.Destroy;
begin
  FLock.Free();
end;

class procedure TPyEnvironmentProjectDeploy.FindPath(out APath,
  AAbsolutePath: string);
begin
  AAbsolutePath := TPyEnvironmentOTAHelper.GetEnvironmentVar(PYTHON_ENVIRONMENT_DIR_VARIABLE, True);
  if IsValidPythonEnvironmentDir(AAbsolutePath) then
    APath :=  '$(' + PYTHON_ENVIRONMENT_DIR_VARIABLE + ')'
  else begin
    APath := '';
    AAbsolutePath := '';
  end;
end;

class function TPyEnvironmentProjectDeploy.GetAbsolutePath: string;
begin
  if not FPathChecked then
    GetPath();
  Result := FAbsolutePath;
end;

class function TPyEnvironmentProjectDeploy.GetDeployFiles(const AProjectName,
  APythonVersion: string; const APlatform: TPyEnvironmentProjectPlatform;
  const ACleaning: boolean)
: TArray<TPyEnvironmentDeployFile>;
var
  LResult: TArray<TPyEnvironmentDeployFile>;
begin
  LResult := nil;

  TPyEnvironmentProjectDeploy.GetDeployFiles(
    AProjectName,
    APythonVersion,
    [APlatform],
    ACleaning,
    // OnFilesReady
    procedure(APlatform: TPyEnvironmentProjectPlatform;
      AFiles: TArray<TPyEnvironmentDeployFile>)
    begin
      LResult := AFiles;
    end,
    // OnException
    procedure(APlatform: TPyEnvironmentProjectPlatform; AException: Exception)
    begin
      ShowException(AException, AException.StackInfo);
    end,
    // OnTerminate
    procedure()
    begin
      //
    end);

  Result := LResult;
end;

class function TPyEnvironmentProjectDeploy.GetFound: Boolean;
begin
  Result := not Path.IsEmpty;
end;

class function TPyEnvironmentProjectDeploy.GetPath: string;
begin
  if not FPathChecked then begin
    FindPath(FPath, FAbsolutePath);
    FPathChecked := True;
  end;

  Result := FPath;
end;

class function TPyEnvironmentProjectDeploy.IsValidPythonEnvironmentDir(
  const APath: string): Boolean;
begin
  Result := TDirectory.Exists(APath);
end;

class procedure TPyEnvironmentProjectDeploy.BeginExecute(
  const APythonVersion: string; const ACleaning: boolean);
begin
  TThread.Synchronize(nil, procedure() begin
    //(BorlandIDEServices as IOTAIDEWaitDialogServices).Show(
    //  'Preparing the Python files.',
    //  'Please, do not operate the IDE meanwhile.');

    Application.MainForm.Enabled := false;

    if ACleaning then
      TPythonMessage.CleaningFiles(APythonVersion)
    else
      TPythonMessage.BuildingFiles(APythonVersion);

    TPythonMessage.WarnControlsLocked();
  end);
end;

class procedure TPyEnvironmentProjectDeploy.EndExecute;
begin
  TThread.Synchronize(nil, procedure() begin
    Application.MainForm.Enabled := true;
    TPythonMessage.AllDone();
    //(BorlandIDEServices as IOTAIDEWaitDialogServices).CloseDialog();
  end);
end;

class procedure TPyEnvironmentProjectDeploy.GetPlatformDeployFiles(
  const AModel: TDeployFilesModel;
  const AFilesReadyCallback: TFilesReadyCallback;
  const AExceptionCallback: TExceptionCallback);
var
  LFiles: TArray<TPyEnvironmentDeployFile>;
begin
  if not MatchText(AModel.PythonVersion, PYTHON_VERSIONS) then
    Exit();

  try
    LFiles := TEnvironmentProjectDeployManager.Execute(AModel);
    // Including files to the deployment list for the given platform
    AFilesReadyCallback(AModel.Platform, LFiles);
  except
    on E: Exception do
      AExceptionCallback(AModel.Platform, E);
  end;
end;

class function TPyEnvironmentProjectDeploy.GetPlatformDeployFilesAsync(
  const AModel: TDeployFilesModel;
  const AFilesReadyCallback: TFilesReadyCallback;
  const AExceptionCallback: TExceptionCallback): ITask;
begin
  Result := TTask.Run(procedure() begin
    GetPlatformDeployFiles(AModel, AFilesReadyCallback, AExceptionCallback)
  end)
end;

class procedure TPyEnvironmentProjectDeploy.GetDeployFiles(
  const AProjectName, APythonVersion: string;
  const APlatforms: TPyEnvironmentProjectPlatforms;
  const ACleaning: boolean;
  const AFilesReadyCallback: TFilesReadyCallback;
  const AExceptionCallback: TExceptionCallback;
  const ATerminatedCallback: TTerminatedCallback);
var
  LModel: TDeployFilesModel;
  LPlatform: TPyEnvironmentProjectPlatform;
begin
  TPythonMessage.ShowMessageView();
  TPythonMessage.Clear();

  try
    for LPlatform in APlatforms do begin
      LModel.ProjectName := AProjectName;
      LModel.Platform := LPlatform;
      LModel.PythonVersion := APythonVersion;
      LModel.PythonEnvironmentDirectory := GetAbsolutePath();
      LModel.Cleaning := ACleaning;

      GetPlatformDeployFiles(LModel, AFilesReadyCallback, AExceptionCallback);
    end;
  finally
    ATerminatedCallback();
  end;
end;

class procedure TPyEnvironmentProjectDeploy.GetDeployFilesAsync(
  const AProjectName, APythonVersion: string;
  const APlatforms: TPyEnvironmentProjectPlatforms;
  const ACleaning: boolean;
  const AFilesReadyCallback: TFilesReadyCallback;
  const AExceptionCallback: TExceptionCallback;
  const ATerminatedCallback: TTerminatedCallback);
var
  LModel: TDeployFilesModel;
begin
  TPythonMessage.ShowMessageView(false);

  // Must queue this call
  TTask.Run(
    procedure()
    var
      LTasks: TArray<ITask>;
      LPlatform: TPyEnvironmentProjectPlatform;
    begin
      System.TMonitor.Enter(FLock);
      try
        TThread.Synchronize(nil, procedure() begin
          TPythonMessage.Clear();
        end);

        BeginExecute(APythonVersion, ACleaning);
        try
          try
            LTasks := nil;
            for LPlatform in APlatforms do begin
              LModel.ProjectName := AProjectName;
              LModel.Platform := LPlatform;
              LModel.PythonVersion := APythonVersion;
              LModel.PythonEnvironmentDirectory := GetAbsolutePath();
              LModel.Cleaning := ACleaning;

              LTasks := LTasks + [
                GetPlatformDeployFilesAsync(
                  LModel, AFilesReadyCallback, AExceptionCallback)];
            end;

            TTask.WaitForAll(LTasks);
          finally
            ATerminatedCallback();
          end;
        finally
          EndExecute();
        end;

        System.TMonitor.Pulse(FLock);
      finally
        System.TMonitor.Exit(FLock);
      end;
    end);
end;

end.
