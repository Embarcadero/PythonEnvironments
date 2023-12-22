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
  System.Generics.Collections,
  DeploymentAPI,
  PyEnvironment.Project.IDE.Types;

type
  TPyEnvironmentProjectDeploy = class
  strict private
    class var
      FDeployableFiles: TDictionary<string, TArray<TPyEnvironmentDeployFile>>;
      FAbsolutePath: string;
      FPath: string;
      FPathChecked: Boolean;
    class procedure FindPath(out APath, AAbsolutePath: string); static;
    class function GetAbsolutePath: string; static;
    class function GetFound: Boolean; static;
    class function GetPath: string; static;
    class function IsValidPythonEnvironmentDir(const APath: string): Boolean; static;
  public
    const
      DEPLOYMENT_CLASS = 'Python';
      PROJECT_USE_PYTHON = 'PYTHON';
      PROJECT_NO_USE_PYTHON = 'NOPYTHON';
      PYTHON_ENVIRONMENT_DIR_VARIABLE = 'PYTHONENVIRONMENTDIR';
      PYTHON_VERSIONS: array[0..4] of string = ('3.8', '3.9', '3.10', '3.11', '3.12');
      SUPPORTED_PLATFORMS = [
        TPyEnvironmentProjectPlatform.Win32, TPyEnvironmentProjectPlatform.Win64,
        TPyEnvironmentProjectPlatform.Android, TPyEnvironmentProjectPlatform.Android64,
        TPyEnvironmentProjectPlatform.iOSSimARM64, TPyEnvironmentProjectPlatform.iOSDevice64,
        TPyEnvironmentProjectPlatform.OSX64, TPyEnvironmentProjectPlatform.OSXARM64,
        TPyEnvironmentProjectPlatform.Linux64];
  public
    class constructor Create();
    class destructor Destroy();

    class function GetDeployFiles(const AProjectName, APythonVersion: string;
      const APlatform: TPyEnvironmentProjectPlatform)
    : TArray<TPyEnvironmentDeployFile>; static;

    class property AbsolutePath: string read GetAbsolutePath;
    class property Found: Boolean read GetFound;
    class property Path: string read GetPath;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.StrUtils,
  PyEnvironment.Project.IDE.Helper,
  PyEnvironment.Project.IDE.Deploy.Platform,
  PyEnvironment.Project.IDE.Deploy.AndroidARM,
  PyEnvironment.Project.IDE.Deploy.AndroidARM64,
  PyEnvironment.Project.IDE.Deploy.OSX64,
  PyEnvironment.Project.IDE.Deploy.OSXARM64,
  PyEnvironment.Project.IDE.Deploy.iOSSimARM64,
  PyEnvironment.Project.IDE.Deploy.iOSDevice64;

{ TPyEnvironmentProject }

class constructor TPyEnvironmentProjectDeploy.Create;
begin
  FDeployableFiles := TDictionary<string, TArray<TPyEnvironmentDeployFile>>.Create();

  FDeployableFiles.Add('3.7', [
    //Windows-win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32, 'python\python3-windows-3.7.9-win32.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Windows-amd64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64, 'python\python3-windows-3.7.9-amd64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Linux-x86_64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64, 'python\python3-linux-3.7.16-x86_64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, '')
  ]);

  FDeployableFiles.Add('3.8', [
    //Windows-win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32, 'python\python3-windows-3.8.10-win32.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Windows-amd64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64, 'python\python3-windows-3.8.10-amd64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Linux-x86_64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64, 'python\python3-linux-3.8.16-x86_64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, '')
  ]);

  FDeployableFiles.Add('3.9', [
    //Windows-win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32, 'python\python3-windows-3.9.13-win32.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Windows-amd64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64, 'python\python3-windows-3.9.13-amd64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Linux-x86_64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64, 'python\python3-linux-3.9.16-x86_64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, '')
  ]);

  FDeployableFiles.Add('3.10', [
    //Windows-win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32, 'python\python3-windows-3.10.9-win32.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Windows-amd64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64, 'python\python3-windows-3.10.9-amd64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Linux-x86_64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64, 'python\python3-linux-3.10.9-x86_64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, '')
  ]);

  FDeployableFiles.Add('3.11', [
    //Windows-win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32, 'python\python3-windows-3.11.2-win32.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Windows-amd64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64, 'python\python3-windows-3.11.2-amd64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Linux-x86_64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64, 'python\python3-linux-3.11.2-x86_64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, '')
  ]);

  FDeployableFiles.Add('3.12', [
    //Windows-win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32, 'python\python3-windows-3.12.0-win32.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Windows-amd64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64, 'python\python3-windows-3.12.0-amd64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, ''),
    //Linux-x86_64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64, 'python\python3-linux-3.12.0-x86_64.zip', '.\', True,  True, TDeployOperation.doCopyOnly, '')
  ]);
end;

class destructor TPyEnvironmentProjectDeploy.Destroy;
begin
  FDeployableFiles.Free();
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
  APythonVersion: string; const APlatform: TPyEnvironmentProjectPlatform)
: TArray<TPyEnvironmentDeployFile>;
var
  I: Integer;
  LAllFiles: TArray<TPyEnvironmentDeployFile>;
begin
  if not MatchText(APythonVersion, PYTHON_VERSIONS) then
    Exit(nil);

  var LPlatformClass: TPyEnvironmentProjectDeployPlatformClass := nil;
  case APlatform of
    Android: LPlatformClass := TPyEnvironmentProjectDeployAndroidARM;
    Android64: LPlatformClass := TPyEnvironmentProjectDeployAndroidARM64;
    iOSDevice64: LPlatformClass := TPyEnvironmentProjectDeployIOSDevice64;
    iOSSimARM64: LPlatformClass := TPyEnvironmentProjectDeployIOSSimARM64;
    OSX64: LPlatformClass := TPyEnvironmentProjectDeployOSX64;
    OSXARM64: LPlatformClass := TPyEnvironmentProjectDeployOSXARM64;
  end;

  if Assigned(LPlatformClass) then
    Exit(LPlatformClass.GetDeployables(
      AProjectName,
      GetAbsolutePath(),
      APythonVersion
    ));

  if not FDeployableFiles.ContainsKey(APythonVersion) then
    Exit(nil);

  LAllFiles := FDeployableFiles[APythonVersion];
  for I := Low(LAllFiles) to High(LAllFiles) do
    if LAllFiles[I].Platform = APlatform then
      Result := Result + [LAllFiles[I]];
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

end.
