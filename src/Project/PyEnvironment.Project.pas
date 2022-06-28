unit PyEnvironment.Project;

interface

uses
  System.Generics.Collections,
  DeploymentAPI,
  PyEnvironment.Project.Types;

type
  TPyEnvironmentProject = class
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
      DEPLOYMENT_CLASS = 'PythonEnvironment';
      PROJECT_USE_PYTHON = 'PYTHON';
      PROJECT_NO_USE_PYTHON = 'NOPYTHON';
      PYTHON_ENVIRONMENT_DIR_VARIABLE = 'PYTHONENVIRONMENTDIR';
      PYTHON_VERSIONS: array[0..3] of string = ('3.7', '3.8', '3.9', '3.10');
      {$IF CompilerVersion < 28} // Below RAD Studio XE7
      SUPPORTED_PLATFORMS = [];
      {$ELSEIF CompilerVersion < 33} // RAD Studio XE7 to RAD Studio 10.2 Tokyo
      SUPPORTED_PLATFORMS = [
        TPyEnvironmentProjectPlatform.Win32, TPyEnvironmentProjectPlatform.Win64];
      {$ELSEIF CompilerVersion < 35} // RAD Studio 10.3 Rio and RAD Studio 10.4 Sydney
      SUPPORTED_PLATFORMS = [
        TPyEnvironmentProjectPlatform.Win32, TPyEnvironmentProjectPlatform.Win64,
        TPyEnvironmentProjectPlatform.Android, TPyEnvironmentProjectPlatform.Android64];
      {$ELSE} // RAD Studio 11 Alexandria and newer
      SUPPORTED_PLATFORMS = [
        TPyEnvironmentProjectPlatform.Win32, TPyEnvironmentProjectPlatform.Win64,
        TPyEnvironmentProjectPlatform.Android, TPyEnvironmentProjectPlatform.Android64,
        //TPyEnvironmentProjectPlatform.iOSDevice64,
        TPyEnvironmentProjectPlatform.OSX64, TPyEnvironmentProjectPlatform.OSXARM64,
        TPyEnvironmentProjectPlatform.Linux64];
      {$ENDIF}
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
  PyEnvironment.Project.Helper;

{ TPyEnvironmentProject }

class constructor TPyEnvironmentProject.Create;
begin
  FDeployableFiles := TDictionary<string, TArray<TPyEnvironmentDeployFile>>.Create();

  FDeployableFiles.Add('3.7', [
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32,     'Python\python3-windows-3.7.9-win32.zip',       '.\',                       True,  True, TDeployOperation.doCopyOnly,   ''), // Win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64,     'Python\python3-windows-3.7.9-amd64.zip',       '.\',                       True,  True, TDeployOperation.doCopyOnly,   ''), // Win64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'Python\python3-android-3.7.13-arm.zip',        '.\assets\internal',        False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'Python\python3-android-3.7.13-arm64.zip',      '.\assets\internal',        False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'Python\python3-android-3.7.13-arm64.zip',      '.\assets\internal',        False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.7.13\arm\libpython3.7m.so',   'library\lib\armeabi-v7a\', False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.7.13\arm64\libpython3.7m.so', 'library\lib\arm64-v8a\',   False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.7.13\arm64\libpython3.7m.so', 'library\lib\armeabi-v7a\', False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.7.13\arm\python3.7',          'library\lib\armeabi-v7a\', False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.7.13\arm64\python3.7',        'library\lib\arm64-v8a\',   False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.7.13\arm64\python3.7',        'library\lib\armeabi-v7a\', False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'Python\python3-macos-3.7.13-x86_64.zip',       'Contents\MacOS\',          False, True, TDeployOperation.doSetExecBit, ''), // OSX64
    //TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'Python\python3-macos-3.7.13-universal2.zip', 'Contents\MacOS\',          False, True, TDeployOperation.doSetExecBit, ''), // OSXARM64 //3.7 is not available for M1
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64,   'Python\python3-linux-3.7.13-x86_64.zip',      '.\',                        False, True, TDeployOperation.doSetExecBit, '')  // Linux64
  ]);

  FDeployableFiles.Add('3.8', [
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32,     'Python\python3-windows-3.8.10-win32.zip',     '.\',                        True,  True, TDeployOperation.doCopyOnly,   ''), // Win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64,     'Python\python3-windows-3.8.10-amd64.zip',     '.\',                        True,  True, TDeployOperation.doCopyOnly,   ''), // Win64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'Python\python3-android-3.8.13-arm.zip',       '.\assets\internal',         False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'Python\python3-android-3.8.13-arm64.zip',     '.\assets\internal',         False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'Python\python3-android-3.8.13-arm64.zip',     '.\assets\internal',         False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.8.13\arm\libpython3.8.so',   'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.8.13\arm64\libpython3.8.so', 'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.8.13\arm64\libpython3.8.so', 'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.8.13\arm\python3.8',         'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.8.13\arm64\python3.8',       'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.8.13\arm64\python3.8',       'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'Python\python3-macos-3.8.13-x86_64.zip',      'Contents\MacOS\',           False, True, TDeployOperation.doSetExecBit, ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'Python\python3-macos-3.8.13-universal2.zip',  'Contents\MacOS\',           False, True, TDeployOperation.doSetExecBit, ''), // OSXARM64 //3.7 is not available for M1
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64,   'Python\python3-linux-3.8.13-x86_64.zip',      '.\',                        False, True, TDeployOperation.doSetExecBit, '')  // Linux64
  ]);

  FDeployableFiles.Add('3.9', [
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32,     'Python\python3-windows-3.9.12-win32.zip',     '.\',                        True,  True, TDeployOperation.doCopyOnly,   ''), // Win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64,     'Python\python3-windows-3.9.12-amd64.zip',     '.\',                        True,  True, TDeployOperation.doCopyOnly,   ''), // Win64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'Python\python3-android-3.9.12-arm.zip',       '.\assets\internal',         False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'Python\python3-android-3.9.12-arm64.zip',     '.\assets\internal',         False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'Python\python3-android-3.9.12-arm64.zip',     '.\assets\internal',         False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.9.12\arm\libpython3.9.so',   'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.9.12\arm64\libpython3.9.so', 'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.9.12\arm64\libpython3.9.so', 'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.9.12\arm\python3.9',         'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.9.12\arm64\python3.9',       'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.9.12\arm64\python3.9',       'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'Python\python3-macos-3.9.12-x86_64.zip',      'Contents\MacOS\',           False, True, TDeployOperation.doSetExecBit, ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'Python\python3-macos-3.9.12-universal2.zip',  'Contents\MacOS\',           False, True, TDeployOperation.doSetExecBit, ''), // OSXARM64 //3.7 is not available for M1
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64,   'Python\python3-linux-3.9.12-x86_64.zip',      '.\',                        False, True, TDeployOperation.doSetExecBit, '')  // Linux64
  ]);

  FDeployableFiles.Add('3.10', [
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win32,     'Python\python3-windows-3.10.4-win32.zip',      '.\',                        True,  True, TDeployOperation.doCopyOnly,   ''), // Win32
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Win64,     'Python\python3-windows-3.10.4-amd64.zip',      '.\',                        True,  True, TDeployOperation.doCopyOnly,   ''), // Win64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'Python\python3-android-3.10.4-arm.zip',        '.\assets\internal',         False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'Python\python3-android-3.10.4-arm64.zip',      '.\assets\internal',         False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'Python\python3-android-3.10.4-arm64.zip',      '.\assets\internal',         False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.10.4\arm\libpython3.10.so',   'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.10.4\arm64\libpython3.10.so', 'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.10.4\arm64\libpython3.10.so', 'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android,   'python\android\3.10.4\arm\python3.10',         'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, ''), // Android
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.10.4\arm64\python3.10',       'library\lib\arm64-v8a\',    False, True, TDeployOperation.doSetExecBit, ''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Android64, 'python\android\3.10.4\arm64\python3.10',       'library\lib\armeabi-v7a\',  False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''), // Android64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSX64,     'Python\python3-macos-3.10.4-x86_64.zip',       'Contents\MacOS\',           False, True, TDeployOperation.doSetExecBit, ''), // OSX64
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.OSXARM64,  'Python\python3-macos-3.10.4-universal2.zip',   'Contents\MacOS\',           False, True, TDeployOperation.doSetExecBit, ''), // OSXARM64 //3.7 is not available for M1
    TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.Linux64,   'Python\python3-linux-3.10.4-x86_64.zip',       '.\',                        False, True, TDeployOperation.doSetExecBit, '')  // Linux64
  ]);
end;

class destructor TPyEnvironmentProject.Destroy;
begin
  FDeployableFiles.Free();
end;

class procedure TPyEnvironmentProject.FindPath(out APath,
  AAbsolutePath: string);
begin
  AAbsolutePath := TPyEnvironmentOTAHelper.GetEnvironmentVar(PYTHON_ENVIRONMENT_DIR_VARIABLE, True);
  if IsValidPythonEnvironmentDir(AAbsolutePath) then
    APath := '$(' + PYTHON_ENVIRONMENT_DIR_VARIABLE + ')'
  else begin
    APath := '';
    AAbsolutePath := '';
  end;
end;

class function TPyEnvironmentProject.GetAbsolutePath: string;
begin
  if not FPathChecked then
    GetPath();
  Result := FAbsolutePath;
end;

class function TPyEnvironmentProject.GetDeployFiles(const APythonVersion: string;
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

class function TPyEnvironmentProject.GetFound: Boolean;
begin
  Result := not Path.IsEmpty;
end;

class function TPyEnvironmentProject.GetPath: string;
begin
  if not FPathChecked then begin
    FindPath(FPath, FAbsolutePath);
    FPathChecked := True;
  end;
  Result := FPath;
end;

class function TPyEnvironmentProject.IsValidPythonEnvironmentDir(
  const APath: string): Boolean;
begin
  Result := TDirectory.Exists(APath);
end;

end.
