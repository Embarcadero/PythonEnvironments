(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Deploy.Platform'              *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality:  Make deployables for custom platforms                 *)
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
unit PyEnvironment.Project.IDE.Deploy.Platform;

interface

uses
  System.SysUtils,
  System.Classes,
  ToolsAPI,
  DeploymentAPI,
  PyEnvironment.Project.IDE.Types,
  PyEnvironment.Project.IDE.Deploy.Intf;

type
  TPyEnvironmentProjectDeployPlatformClass = class of TPyEnvironmentProjectDeployPlatform;

  TPyEnvironmentProjectDeployPlatform = class(TInterfacedObject, IDeploymentTask)
  private
    FModel: TDeployFilesModel;
    FStartTaskCallback: TDeployTaskStartCallback;
    FFinishTaskCallback: TDeployTaskFinishCallback;
    FProgressCallback: TDeployTaskProgressCallback;
  private
    function MakeStartTaskMessage(const ADeployTask: TDeployTask): string;
  protected
    function GetPythonVersion: string; inline;
    function GetProjectFolder: string; inline;
    function GetProjectName: string; inline;
    function GetEnvironmentFolder: string; inline;
    function GetBundleImageFolder: string; inline;
    function GetBundleMinimalFileName: string; inline;

    function GetPlatform: TPyEnvironmentProjectPlatform; virtual; abstract;
    function GetPythonBundleName: string; virtual; abstract;
    function GetBundleMinimalIgnoresList: TArray<string>; virtual;
  protected
    /// <summary>
    /// Locate the Python bundle by version.
    /// </summary>
    function LocatePythonBundle: string;
  private
    // IDeployOperation implementation
    function GetStartTaskCallback: TDeployTaskStartCallback;
    procedure SetStartTaskCallback(Value: TDeployTaskStartCallback);
    function GetFinishTaskCallback: TDeployTaskFinishCallback;
    procedure SetFinishTaskCallback(Value: TDeployTaskFinishCallback);
    function GetProgressCallback: TDeployTaskProgressCallback;
    procedure SetProgressCallback(Value: TDeployTaskProgressCallback);
  protected
    // IDeployOperation implementation
    function ShouldDownload(): boolean;
    function ShouldMake(): boolean;

    function Download(const AInput: TDeployTaskInput): TDeployTaskOutput; virtual;
    function Make(const AInput: TDeployTaskInput): TDeployTaskOutput; virtual; abstract;
    function Deploy(const AInput: TDeployTaskInput): TDeployTaskOutput; virtual; abstract;
    function Clean(const AInput: TDeployTaskInput): TDeployTaskOutput; virtual;

    function Execute(out AFiles: TPyEnvironmentDeployFiles): boolean;
  public
    constructor Create(const AModel: TDeployFilesModel);

    property Model: TDeployFilesModel read FModel;
    property OnStartTask: TDeployTaskStartCallback read GetStartTaskCallback write SetStartTaskCallback;
    property OnFinishTask: TDeployTaskFinishCallback read GetFinishTaskCallback write SetFinishTaskCallback;
    property OnProgress: TDeployTaskProgressCallback read GetProgressCallback write SetProgressCallback;
  end;

implementation

uses
  System.Zip,
  System.IOUtils,
  System.Threading,
  PyEnvironment.Project.IDE.DownloadBundle;

{ TPyEnvironmentProjectDeployPlatform }

constructor TPyEnvironmentProjectDeployPlatform.Create(
  const AModel: TDeployFilesModel);
begin
  inherited Create();
  FModel := AModel;
end;

function TPyEnvironmentProjectDeployPlatform.GetProgressCallback: TDeployTaskProgressCallback;
begin
  Result := FProgressCallback;
end;

procedure TPyEnvironmentProjectDeployPlatform.SetProgressCallback(
  Value: TDeployTaskProgressCallback);
begin
  FProgressCallback := Value;
end;

function TPyEnvironmentProjectDeployPlatform.GetStartTaskCallback: TDeployTaskStartCallback;
begin
  Result := FStartTaskCallback;
end;

procedure TPyEnvironmentProjectDeployPlatform.SetStartTaskCallback(
  Value: TDeployTaskStartCallback);
begin
  FStartTaskCallback := Value;
end;

function TPyEnvironmentProjectDeployPlatform.GetFinishTaskCallback: TDeployTaskFinishCallback;
begin
  Result := FFinishTaskCallback;
end;

procedure TPyEnvironmentProjectDeployPlatform.SetFinishTaskCallback(
  Value: TDeployTaskFinishCallback);
begin
  FFinishTaskCallback := Value;
end;

function TPyEnvironmentProjectDeployPlatform.GetProjectFolder: string;
begin
  Result := TPath.GetDirectoryName(FModel.ProjectName);
end;

function TPyEnvironmentProjectDeployPlatform.GetProjectName: string;
begin
  Result := TPath.GetFileName(FModel.ProjectName);
end;

function TPyEnvironmentProjectDeployPlatform.GetPythonVersion: string;
begin
  Result := Model.PythonVersion;
end;

function TPyEnvironmentProjectDeployPlatform.GetEnvironmentFolder: string;
begin
  Result := FModel.PythonEnvironmentDirectory;
end;

function TPyEnvironmentProjectDeployPlatform.GetBundleImageFolder: string;
begin
  Result := TPath.Combine(GetEnvironmentFolder(), 'python');
end;

function TPyEnvironmentProjectDeployPlatform.GetBundleMinimalFileName: string;
begin
  Result := TPath.Combine(
    TPath.Combine(GetEnvironmentFolder(), 'python'),
    'min-' + TPath.GetFileName(LocatePythonBundle()));
end;

function TPyEnvironmentProjectDeployPlatform.GetBundleMinimalIgnoresList: TArray<string>;
begin
  Result := [
    // Remove 2to3
    'bin/2to3*',
    // Remove IDLE
    'bin/idle*',
    // Remove pydoc
    'bin/pydoc*',
    // Remove pythonxx-config
    'bin/python*-config',
    // We don't need C headers.
    'include/*',
    // No man use.
    'share/*',
    // Remove standard library test suites.
    'lib/python3.*/ctypes/test/*', 'lib/python3.*/distutils/tests/*', 'lib/python3.*/lib2to3/tests/*', 'lib/python3.*/sqlite3/test/*', 'lib/python3.*/test/*',
    // Remove config-* directory, which is used for compiling C extension modules.
    'lib/python3.*/config-*',
    // Remove pydoc
    'lib/pydoc_data',
    // Remove ensurepip. If user code needs pip, it can add it to
    'lib/python3.*/ensurepip/*',
    // Remove libraries supporting IDLE. We don't need to ship an IDE
    'lib/python3.*/idlelib/*',
    // Remove Tcl/Tk GUI code.
    'lib/python3.*/tkinter/*', 'lib/python3.*/turtle.py', 'lib/python3.*/turtledemo/*',
    // Remove sysconfigdata
    'lib/python3.*/_sysconfigdata/*',
    // Remove command-line curses toolkit.
    'lib/python3.*/curses/*',
    // Remove pyc files. These take up space, but since most stdlib modules are never imported by user code, they mostly have no value.
    '*/__pycache__/*',
    // Remove pkgconfig
    'lib/pkgconfig*',
    // Remove the static lib
    'lib/*.a'
  ];
end;

function TPyEnvironmentProjectDeployPlatform.LocatePythonBundle: string;
begin
  Result := TPath.Combine(GetBundleImageFolder(), GetPythonBundleName());
end;

function TPyEnvironmentProjectDeployPlatform.MakeStartTaskMessage(
  const ADeployTask: TDeployTask): string;
begin
  case ADeployTask of
    TDeployTask.Download: Result := 'Downloading ';
    TDeployTask.Make: Result := 'Making ';
    TDeployTask.Deploy: Result := 'Deploying ';
    TDeployTask.Clean: Result := 'Cleaning ';
    else
      Result := String.Empty;
  end;
  Result := Result + GetPythonBundleName() + ' for ' + GetPlatform().ToString();
end;

function TPyEnvironmentProjectDeployPlatform.ShouldDownload: boolean;
var
  LFileName: string;
begin
  LFileName := LocatePythonBundle();

  Result := not (TFile.Exists(LFileName)
    and TZipFile.IsValid(LFileName));
end;

function TPyEnvironmentProjectDeployPlatform.ShouldMake: boolean;
var
  LFileName: string;
begin
  LFileName := GetBundleMinimalFileName();

  Result := not (TFile.Exists(LFileName)
    and TZipFile.IsValid(LFileName));
end;

function TPyEnvironmentProjectDeployPlatform.Download(
  const AInput: TDeployTaskInput): TDeployTaskOutput;
var
  LFileName: string;
begin
  LFileName := LocatePythonBundle();

  Result := TDownloadPythonBundle.Download(GetPythonBundleName(), LFileName,
    procedure(APercentage: integer) begin
      OnProgress(TDeployTask.Download, APercentage);
    end);

   Result.Success := Result.Success and TFile.Exists(LFileName);
end;

function TPyEnvironmentProjectDeployPlatform.Clean(
  const AInput: TDeployTaskInput): TDeployTaskOutput;
begin
  // We are basing in the exact same files deployed
  Result := Deploy(AInput);
end;

function TPyEnvironmentProjectDeployPlatform.Execute(
  out AFiles: TPyEnvironmentDeployFiles): boolean;
var
  LInput: TDeployTaskInput;
  LOutput: TDeployTaskOutput;
begin
  LInput := Default(TDeployTaskInput);

  if ShouldDownload() then begin
    OnStartTask(TDeployTask.Download, MakeStartTaskMessage(TDeployTask.Download));
    try
      LOutput := Download(LInput);
      if not LOutput.Success then
        Exit(false);
    finally
      OnFinishTask(TDeployTask.Download, LOutput);
    end;
  end;

  if ShouldMake() then begin
    OnStartTask(TDeployTask.Make, MakeStartTaskMessage(TDeployTask.Make));
    try
      LOutput := Make(LInput);
      if not LOutput.Success then
        Exit(false);
    finally
      OnFinishTask(TDeployTask.Make, LOutput);
    end;
  end;

  if not FModel.Cleaning then begin
    OnStartTask(TDeployTask.Deploy, MakeStartTaskMessage(TDeployTask.Deploy));
    try
      LOutput := Deploy(LInput);
      if not LOutput.Success then
        Exit(false);

      AFiles := LOutput.Args.GetFiles();
    finally
      OnFinishTask(TDeployTask.Deploy, LOutput);
    end;
  end else begin
    OnStartTask(TDeployTask.Clean, MakeStartTaskMessage(TDeployTask.Clean));
    try
      LOutput := Clean(LInput);
      if not LOutput.Success then
        Exit(false);

      AFiles := LOutput.Args.GetFiles();
    finally
      OnFinishTask(TDeployTask.Clean, LOutput);
    end;
  end;

  Result := true;
end;

end.
