(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Deploy.Android'               *)
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
unit PyEnvironment.Project.IDE.Deploy.Android;

interface

uses
  System.SysUtils,
  System.Classes,
  ToolsAPI,
  DeploymentAPI,
  PyEnvironment.Project.IDE.Types,
  PyEnvironment.Project.IDE.Deploy.Platform;

type
  TPyEnvironmentProjectDeployAndroid = class(TPyEnvironmentProjectDeployPlatform)
  private
    function GetAssetsFolder(const APythonVersion: string): string; inline;
    function GetExistingAssets: TArray<string>;
  private
    /// <summary>
    /// Opens the Python distribution .zip collecting the .so files
    /// to make assets.
    /// </summary>
    function MakeAssetsFromZip: TArray<string>;
    /// <summary>
    /// Delete all assets.
    /// </summary>
    procedure ClearAssets;
    /// <summary>
    /// The minimal Python bundle to be distributed to Android.
    /// Excludes many unnecessary files.
    /// </summary>
    function MakePythonMinimalBundle: string;
  protected
    function GetBundleMinimalIgnoresList: TArray<string>; override;
    function Build: TArray<TPyEnvironmentDeployFile>; override;
  end;


implementation

uses
  System.IOUtils,
  System.Masks,
  System.Zip,
  PyEnvironment.Project.IDE.Deploy;

{ TPyEnvironmentProjectDeployAndroid }

function TPyEnvironmentProjectDeployAndroid.GetAssetsFolder(
  const APythonVersion: string): string;
begin
  // Deploy assets to the project's build folder
  var LConfigFolder := (BorlandIDEServices as IOTAServices)
    .ExpandRootMacro('$(Config)');

  Result := TPath.Combine(
    GetProjectFolder(),
    GetPlatform().ToString(),
    LConfigFolder,
    TPath.GetFileNameWithoutExtension(ProjectFileName)
    + '.'
    + 'python' + APythonVersion
    + '.'
    + 'Assets');
end;

function TPyEnvironmentProjectDeployAndroid.GetBundleMinimalIgnoresList: TArray<string>;
begin
  Result := inherited + [
    'bin/*',
    // Remove the Python shared libs
    'lib/libpython*.so*'
  ];
end;

function TPyEnvironmentProjectDeployAndroid.GetExistingAssets: TArray<string>;
begin
  Result := TDirectory.GetFiles(GetAssetsFolder(PythonVersion),
    TSearchOption.soAllDirectories,
    function(const AFileName: string; const SearchRec: TSearchRec): boolean
    begin
      Result := String(SearchRec.Name).EndsWith('.so');
    end);
end;

procedure TPyEnvironmentProjectDeployAndroid.ClearAssets;
begin
  for var LPythonVersion in TPyEnvironmentProjectDeploy.PYTHON_VERSIONS do begin
    var LAssetsFolder := GetAssetsFolder(LPythonVersion);
    if TDirectory.Exists(LAssetsFolder) then
      TDirectory.Delete(LAssetsFolder, true);
  end;
end;

function TPyEnvironmentProjectDeployAndroid.MakeAssetsFromZip: TArray<string>;
var
  LStream: TStream;
  LLocalHeader: TZipHeader;
begin
  if TDirectory.Exists(GetAssetsFolder(PythonVersion)) then
    Exit(GetExistingAssets());

  // It is the first time set or something has changed. Let's clean up.
  ClearAssets();

  // Make the assets folder
  TDirectory.CreateDirectory(GetAssetsFolder(PythonVersion));

  var LZip := TZipFile.Create;
  try
    LZip.Open(LocatePythonBundle(), TZipMode.zmRead);

    // Is the libpythonxx.so.1.0 the real one?
    if LZip.IndexOf(Format('lib/libpython%s.so.1.0', [PythonVersion])) >= 0 then begin
      var LAssetFileName := TPath.Combine(
        GetAssetsFolder(PythonVersion),
        Format('libpython%s.so', [PythonVersion]));

      if TFile.Exists(LAssetFileName) then
        TFile.Delete(LAssetFileName);

      var LAssetStream := TFileStream.Create(LAssetFileName, fmCreate);
      try
        LZip.Read(Format('lib/libpython%s.so.1.0', [PythonVersion]), LStream, LLocalHeader);
        LAssetStream.CopyFrom(LStream, LStream.Size);
      finally
        LAssetStream.Free;
      end;

      Result := Result + [LAssetFileName];
    end;

    // We need to name the python launcher with the "lib" prefix
    var LAssetFileName := TPath.Combine(
      GetAssetsFolder(PythonVersion),
      Format('libpythonlauncher%s.so', [PythonVersion]));
    var LAssetStream := TFileStream.Create(LAssetFileName, fmCreate);
    try
      LZip.Read('bin/python' + PythonVersion, LStream, LLocalHeader);
      LAssetStream.CopyFrom(LStream, LStream.Size);
    finally
      LAssetStream.Free;
    end;

    Result := Result + [LAssetFileName];

    LZip.Close;
  finally
    LZip.Free;
  end;
end;

function TPyEnvironmentProjectDeployAndroid.MakePythonMinimalBundle: string;

  function ShouldIgnore(const AFileName: string): boolean;
  begin
    for var LIgnore in GetBundleMinimalIgnoresList() do
      if MatchesMask(AFileName, LIgnore) then
        Exit(true);

    Result := false;
  end;

  procedure ReadImage(const APythonBundle: string; const ACallback: TProc<TStream, TZipHeader, TFileName>);
  var
    LStream: TStream;
    LLocalHeader: TZipHeader;
  begin
    Assert(Assigned(ACallback), 'Parameter "ACallback" not assigned.');

    var LZipReader := TZipFile.Create;
    try
      LZipReader.Open(APythonBundle, TZipMode.zmRead);
      for var LFileName in LZipReader.FileNames do
      begin
        if ShouldIgnore(LFileName) then
          Continue;

        LZipReader.Read(LFileName, LStream, LLocalHeader);
        ACallback(LStream, LLocalHeader, LFileName);
      end;
      LZipReader.Close;
    finally
      LZipReader.Free;
    end;
  end;

begin
  // Create a minimal Python bundle
  Result := GetBundleMinimalFileName();

  // Always rebuild ??? Not now...
  if TFile.Exists(Result) then
    Exit(Result);

  if not TDirectory.Exists(TPath.GetDirectoryName(Result)) then
    TDirectory.CreateDirectory(TPath.GetDirectoryName(Result));

  var LZipWriter := TZipFile.Create();
  try
    LZipWriter.Open(Result, TZipMode.zmWrite);
    // Iterate over the image bundle and create the minimal bundle
    ReadImage(LocatePythonBundle(),
      procedure(AData: TStream; AZipHeader: TZipHeader; AFileName: TFileName) begin
        if not String(AFileName).IsEmpty then
          LZipWriter.Add(
            AData,
            AFileName);
      end);
    LZipWriter.Close;
  finally
    LZipWriter.Free;
  end;
end;

function TPyEnvironmentProjectDeployAndroid.Build: TArray<TPyEnvironmentDeployFile>;
begin
  // Add assets to the deploy file list
  for var LAsset: string in MakeAssetsFromZip() do
  begin
    var LDeployOp := TDeployOperation.doCopyOnly;
    if LAsset.EndsWith('.so') then
      LDeployOp := TDeployOperation.doSetExecBit;

    var LDestFolder := 'library\lib\armeabi-v7a\';
    if GetPlatform() = TPyEnvironmentProjectPlatform.Android64 then
      LDestFolder := 'library\lib\arm64-v8a\';

    Result := Result + [
      TPyEnvironmentDeployFile.Create(GetPlatform(),
        // Make the asset path relative to project's path
        LAsset.Replace(IncludeTrailingPathDelimiter(GetProjectFolder()), '', []),
        LDestFolder,
        true,  true, LDeployOp, '', false)
    ];
  end;

  // Add the python minimal bundle to the deploy file list
  var LPythonLibZip := MakePythonMinimalBundle();
  Result := Result + [
    TPyEnvironmentDeployFile.Create(GetPlatform(),
      LPythonLibZip.Replace(IncludeTrailingPathDelimiter(PythonEnvironmentFolder), '', []),
      // Extracts to ./PYVER by default
      '.\assets\internal',
      false, true,
      TDeployOperation.doCopyOnly, //It seems Android doesn't support doUnArchive :(
      '')
  ];
end;

end.
