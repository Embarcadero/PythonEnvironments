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
      PYTHON_VERSIONS: array[0..3] of string = ('3.7', '3.8', '3.9', '3.10');
      SUPPORTED_PLATFORMS = [
        TPyEnvironmentProjectPlatform.Win32, TPyEnvironmentProjectPlatform.Win64,
        TPyEnvironmentProjectPlatform.Android, TPyEnvironmentProjectPlatform.Android64,
        //TPyEnvironmentProjectPlatform.iOSDevice64,
        TPyEnvironmentProjectPlatform.OSX64, TPyEnvironmentProjectPlatform.OSXARM64,
        TPyEnvironmentProjectPlatform.Linux64];
  public
    class constructor Create();
    class destructor Destroy();

    class function GetDeployFiles(const APythonVersion: string; const APlatform: TPyEnvironmentProjectPlatform): TArray<TPyEnvironmentDeployFile>; static;
    class property AbsolutePath: string read GetAbsolutePath;
    class property Found: Boolean read GetFound;
    class property Path: string read GetPath;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  PyEnvironment.Project.IDE.Helper;

{ TPyEnvironmentProject }

class constructor TPyEnvironmentProjectDeploy.Create;
begin
  FDeployableFiles := TDictionary<string, TArray<TPyEnvironmentDeployFile>>.Create();

  FDeployableFiles.Add('3.7', [
    //Windows
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32,     'python\python3-windows-3.7.9-win32.zip',       '.\',                       True,  True, TDeployOperation.doCopyOnly,   ''), // Win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64,     'python\python3-windows-3.7.9-amd64.zip',       '.\',                       True,  True, TDeployOperation.doCopyOnly,   ''), // Win64
    //Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\python3-android-3.7.15-arm.zip',        '.\assets\internal',        False, True, TDeployOperation.doCopyOnly,   ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\python3-android-3.7.15-arm64.zip',      '.\assets\internal',        False, True, TDeployOperation.doCopyOnly,   ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\python3-android-3.7.15-arm.zip',        '.\assets\internal',        False, True, TDeployOperation.doCopyOnly,   '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.7.15\arm\libpython3.7m.so',   'library\lib\armeabi-v7a\', False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.7.15\arm64\libpython3.7m.so', 'library\lib\arm64-v8a\',   False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.7.15\arm\libpython3.7m.so',   'library\lib\armeabi-v7a\', False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.7.15\arm\python3.7m',         'library\lib\armeabi-v7a\', False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.7.15\arm64\python3.7m',       'library\lib\arm64-v8a\',   False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.7.15\arm\python3.7m',         'library\lib\armeabi-v7a\', False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    //MacOS
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\python3-macos-3.7.15-x86_64.zip',       'Contents\Resources\',      True,  True, TDeployOperation.doCopyOnly,   ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\macos\3.7.15\intel\libpython3.7m.dylib','Contents\MacOS\',          True,  True, TDeployOperation.doSetExecBit, ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\macos\3.7.15\intel\python3.7m',         'Contents\MacOS\',          True,  True, TDeployOperation.doSetExecBit, ''), // OSX64
    //TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'python\python3-macos-3.7.13-universal2.zip',   'Contents\MacOS\',          True,  True, TDeployOperation.doCopyOnly,  ''), // OSXARM64 //3.7 is not available for M1
    //Linux
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64,   'python\python3-linux-3.7.15-x86_64.zip',       '.\',                       True,  True, TDeployOperation.doCopyOnly,   '')  // Linux64
  ]);

  FDeployableFiles.Add('3.8', [
    //Windows
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32,     'python\python3-windows-3.8.10-win32.zip',     '.\',                        True,  True, TDeployOperation.doCopyOnly,   ''), // Win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64,     'python\python3-windows-3.8.10-amd64.zip',     '.\',                        True,  True, TDeployOperation.doCopyOnly,   ''), // Win64
    //Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\python3-android-3.8.15-arm.zip',       '.\assets\internal',         False, True, TDeployOperation.doCopyOnly,   ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\python3-android-3.8.15-arm64.zip',     '.\assets\internal',         False, True, TDeployOperation.doCopyOnly,   ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\python3-android-3.8.15-arm.zip',       '.\assets\internal',         False, True, TDeployOperation.doCopyOnly,   '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.8.15\arm\libpython3.8.so',   'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.8.15\arm64\libpython3.8.so', 'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.8.15\arm\libpython3.8.so',   'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.8.15\arm\python3.8',         'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.8.15\arm64\python3.8',       'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.8.15\arm\python3.8',         'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    //MacOS
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\python3-macos-3.8.15-x86_64.zip',      'Contents\Resources\',       True,  True, TDeployOperation.doCopyOnly,   ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'python\python3-macos-3.8.15-universal2.zip',  'Contents\Resources\',       True,  True, TDeployOperation.doCopyOnly,   ''), // OSXARM64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\macos\3.8.15\intel\libpython3.8.dylib','Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\macos\3.8.15\intel\python3.8',         'Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'python\macos\3.8.15\arm\libpython3.8.dylib',  'Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSXARM64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'python\macos\3.8.15\arm\python3.8',           'Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSXARM64
    //Linux
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64,   'python\python3-linux-3.8.15-x86_64.zip',      '.\',                        True,  True, TDeployOperation.doCopyOnly,   '')  // Linux64
  ]);

  FDeployableFiles.Add('3.9', [
    //Windows
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32,     'python\python3-windows-3.9.13-win32.zip',     '.\',                        True,  True, TDeployOperation.doCopyOnly,   ''), // Win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64,     'python\python3-windows-3.9.13-amd64.zip',     '.\',                        True,  True, TDeployOperation.doCopyOnly,   ''), // Win64
    //Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\python3-android-3.9.15-arm.zip',       '.\assets\internal',         False, True, TDeployOperation.doCopyOnly,   ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\python3-android-3.9.15-arm64.zip',     '.\assets\internal',         False, True, TDeployOperation.doCopyOnly,   ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\python3-android-3.9.15-arm.zip',       '.\assets\internal',         False, True, TDeployOperation.doCopyOnly,   '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.9.15\arm\libpython3.9.so',   'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.9.15\arm64\libpython3.9.so', 'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.9.15\arm\libpython3.9.so',   'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.9.15\arm\python3.9',         'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.9.15\arm64\python3.9',       'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.9.15\arm\python3.9',         'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    //MacOS
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\python3-macos-3.9.15-x86_64.zip',      'Contents\Resources\',       True,  True, TDeployOperation.doCopyOnly,   ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'python\python3-macos-3.9.15-universal2.zip',  'Contents\Resources\',       True,  True, TDeployOperation.doCopyOnly,   ''), // OSXARM64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\macos\3.9.15\intel\libpython3.9.dylib','Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\macos\3.9.15\intel\python3.9',         'Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'python\macos\3.9.15\arm\libpython3.9.dylib',  'Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSXARM64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'python\macos\3.9.15\arm\python3.9',           'Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSXARM64
    //Linux
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64,   'python\python3-linux-3.9.15-x86_64.zip',      '.\',                        True,  True, TDeployOperation.doCopyOnly,   '')  // Linux64
  ]);

  FDeployableFiles.Add('3.10', [
    //Windows
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32,     'python\python3-windows-3.10.8-win32.zip',      '.\',                        True,  True, TDeployOperation.doCopyOnly,  ''), // Win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64,     'python\python3-windows-3.10.8-amd64.zip',      '.\',                        True,  True, TDeployOperation.doCopyOnly,  ''), // Win64
    //Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\python3-android-3.10.8-arm.zip',        '.\assets\internal',         False, True, TDeployOperation.doCopyOnly,  ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\python3-android-3.10.8-arm64.zip',      '.\assets\internal',         False, True, TDeployOperation.doCopyOnly,  ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\python3-android-3.10.8-arm.zip',        '.\assets\internal',         False, True, TDeployOperation.doCopyOnly,  '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.10.8\arm\libpython3.10.so',   'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.10.8\arm64\libpython3.10.so', 'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.10.8\arm\libpython3.10.so',   'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.10.8\arm\python3.10',         'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.10.8\arm64\python3.10',       'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.10.8\arm\python3.10',         'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    //MacOS
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\python3-macos-3.10.8-x86_64.zip',       'Contents\Resources\',       True,  True, TDeployOperation.doCopyOnly,   ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'python\python3-macos-3.10.8-universal2.zip',   'Contents\Resources\',       True,  True, TDeployOperation.doCopyOnly,   ''), // OSXARM64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\macos\3.10.8\intel\libpython3.10.dylib','Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'python\macos\3.10.8\intel\python3.10',         'Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'python\macos\3.10.8\arm\libpython3.10.dylib',  'Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSXARM64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'python\macos\3.10.8\arm\python3.10',           'Contents\MacOS\',           True,  True, TDeployOperation.doSetExecBit, ''), // OSXARM64
    //Linux
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64,   'python\python3-linux-3.10.8-x86_64.zip',       '.\',                        True,  True, TDeployOperation.doCopyOnly,   '')  // Linux64
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

class function TPyEnvironmentProjectDeploy.GetDeployFiles(const APythonVersion: string;
  const APlatform: TPyEnvironmentProjectPlatform): TArray<TPyEnvironmentDeployFile>;
var
  I: Integer;
  LAllFiles: TArray<TPyEnvironmentDeployFile>;
begin
  Result := [];

  if not FDeployableFiles.ContainsKey(APythonVersion) then
    Exit();

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
