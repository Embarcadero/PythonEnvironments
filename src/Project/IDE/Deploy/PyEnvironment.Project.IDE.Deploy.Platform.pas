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
  PyEnvironment.Project.IDE.Types;

type
  TPyEnvironmentProjectDeployPlatformClass = class of TPyEnvironmentProjectDeployPlatform;

  TPyEnvironmentProjectDeployPlatform = class
  private
    FProjectFileName: string;
    FPythonEnvironmentFolder: string;
    FPythonVersion: string;
  protected
    function GetProjectFolder: string; inline;
    function GetProjectName: string; inline;
    function GetBundleImageFolder: string; inline;
    function GetBundleMinimalFileName: string; inline;

    function GetPlatform: TPyEnvironmentProjectPlatform; virtual; abstract;
    function GetPythonBundleName: string; virtual; abstract;
    function GetBundleMinimalIgnoresList: TArray<string>; virtual;

    function Build: TArray<TPyEnvironmentDeployFile>; virtual; abstract;
  protected
    /// <summary>
    /// Locate the Python bundle by version.
    /// </summary>
    function LocatePythonBundle: string;
  public
    constructor Create(const AProjectFileName, APythonEnvironmentFolder,
      APythonVersion: string);

    class function GetDeployables(const AProjectFileName, APythonEnvironmentDir,
      APythonVersion: string): TArray<TPyEnvironmentDeployFile>;

    property PythonVersion: string read FPythonVersion;
    property ProjectFileName: string read FProjectFileName;
    property PythonEnvironmentFolder: string read FPythonEnvironmentFolder;
  end;


implementation

uses
  System.IOUtils;

{ TPyEnvironmentProjectDeployPlatform }

constructor TPyEnvironmentProjectDeployPlatform.Create(const AProjectFileName,
  APythonEnvironmentFolder, APythonVersion: string);
begin
  inherited Create();
  FProjectFileName := AProjectFileName;
  FPythonEnvironmentFolder := APythonEnvironmentFolder;
  FPythonVersion := APythonVersion;
end;

function TPyEnvironmentProjectDeployPlatform.GetProjectFolder: string;
begin
  Result := TPath.GetDirectoryName(FProjectFileName);
end;

function TPyEnvironmentProjectDeployPlatform.GetProjectName: string;
begin
  Result := TPath.GetFileName(FProjectFileName);
end;

function TPyEnvironmentProjectDeployPlatform.GetBundleImageFolder: string;
begin
  Result := TPath.Combine(PythonEnvironmentFolder, 'python');
end;

function TPyEnvironmentProjectDeployPlatform.GetBundleMinimalFileName: string;
begin
  Result := TPath.Combine(PythonEnvironmentFolder, 'python',
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

class function TPyEnvironmentProjectDeployPlatform.GetDeployables(
  const AProjectFileName, APythonEnvironmentDir,
  APythonVersion: string): TArray<TPyEnvironmentDeployFile>;
begin
  var LProjectDeploy := TPyEnvironmentProjectDeployPlatformClass(Self).Create(
    AProjectFileName, APythonEnvironmentDir, APythonVersion);
  try
    Result := LProjectDeploy.Build();
  finally
    LProjectDeploy.Free();
  end;
end;

end.
