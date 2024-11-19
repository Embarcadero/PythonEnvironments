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
  protected
    function GetBundleMinimalIgnoresList: TArray<string>; override;
  protected
    function Make(const AInput: TDeployTaskInput): TDeployTaskOutput; override;
    function Deploy(const AInput: TDeployTaskInput): TDeployTaskOutput; override;
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

  Result := TPath.Combine(GetProjectFolder(), GetPlatform().ToString());

  Result := TPath.Combine(Result, LConfigFolder);

  Result := TPath.Combine(
    Result,
    TPath.GetFileNameWithoutExtension(GetProjectName())
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
  Result := TDirectory.GetFiles(GetAssetsFolder(GetPythonVersion()),
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
  if TDirectory.Exists(GetAssetsFolder(GetPythonVersion())) then
    Exit(GetExistingAssets());

  // It is the first time set or something has changed. Let's clean up.
  ClearAssets();

  // Make the assets folder
  TDirectory.CreateDirectory(GetAssetsFolder(GetPythonVersion()));

  var LZip := TZipFile.Create;
  try
    LZip.Open(LocatePythonBundle(), TZipMode.zmRead);

    // Is the libpythonxx.so.1.0 the real one?
    var LLibFileName := String.Empty;
    if LZip.IndexOf(Format('lib/libpython%s.so.1.0', [GetPythonVersion()])) >= 0 then
      LLibFileName := 'lib/libpython%s.so.1.0'
    else if LZip.IndexOf(Format('lib/libpython%s.so', [GetPythonVersion()])) >= 0 then
      LLibFileName := 'lib/libpython%s.so';

    if not LLibFileName.IsEmpty() then begin
      var LAssetFileName := TPath.Combine(
        GetAssetsFolder(GetPythonVersion()),
        Format('libpython%s.so', [GetPythonVersion()]));

      if TFile.Exists(LAssetFileName) then
        TFile.Delete(LAssetFileName);

      var LAssetStream := TFileStream.Create(LAssetFileName, fmCreate);
      try
        LZip.Read(Format(LLibFileName, [GetPythonVersion()]), LStream, LLocalHeader);
        LAssetStream.CopyFrom(LStream, LStream.Size);
      finally
        LAssetStream.Free;
      end;

      Result := Result + [LAssetFileName];
    end;

    // We need to name the python launcher with the "lib" prefix
    var LAssetFileName := TPath.Combine(
      GetAssetsFolder(GetPythonVersion()),
      Format('libpythonlauncher%s.so', [GetPythonVersion()]));
    var LAssetStream := TFileStream.Create(LAssetFileName, fmCreate);
    try
      LZip.Read('bin/python' + GetPythonVersion(), LStream, LLocalHeader);
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

function TPyEnvironmentProjectDeployAndroid.Make(
  const AInput: TDeployTaskInput): TDeployTaskOutput;

  function ShouldIgnore(const AFileName: string): boolean;
  begin
    for var LIgnore in GetBundleMinimalIgnoresList() do
      if MatchesMask(AFileName, LIgnore) then
        Exit(true);

    Result := false;
  end;

  procedure ReadImage(const APythonBundle: string; const ACallback: TProc<TStream, TZipHeader, TFileName>);
  var
    LZipReader: TZipFile;
    LStream: TStream;
    LLocalHeader: TZipHeader;
    LFileName: string;
  begin
    Assert(Assigned(ACallback), 'Parameter "ACallback" not assigned.');

    LZipReader := TZipFile.Create;
    try
      LZipReader.Open(APythonBundle, TZipMode.zmRead);
      for LFileName in LZipReader.FileNames do
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

  var LZipWriter := TZipFile.Create();
  try
    LZipWriter.Open(LFileName, TZipMode.zmWrite);
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

  Result := TFile.Exists(LFileName);
end;

function TPyEnvironmentProjectDeployAndroid.Deploy(
  const AInput: TDeployTaskInput): TDeployTaskOutput;
var
  LFileName: string;
  LFiles: TPyEnvironmentDeployFiles;
begin
  LFileName := GetBundleMinimalFileName();
  if not TFile.Exists(LFileName) then
    Exit(false);

  LFiles := [];

  // Add assets to the deploy file list
  for var LAsset: string in MakeAssetsFromZip() do
  begin
    var LDeployOp := TDeployOperation.doCopyOnly;
    if LAsset.EndsWith('.so') then
      LDeployOp := TDeployOperation.doSetExecBit;

    var LDestFolder := 'library\lib\armeabi-v7a\';
    if GetPlatform() = TPyEnvironmentProjectPlatform.Android64 then
      LDestFolder := 'library\lib\arm64-v8a\';

    LFiles := LFiles + [
      TPyEnvironmentDeployFile.Create(GetPlatform(),
        // Make the asset path relative to project's path
        LAsset.Replace(IncludeTrailingPathDelimiter(GetProjectFolder()), '', []),
        LDestFolder,
        true,  true, LDeployOp, '', false)
    ];
  end;

  // Add the python minimal bundle to the deploy file list
  LFiles := LFiles + [
    TPyEnvironmentDeployFile.Create(GetPlatform(),
      LFileName.Replace(IncludeTrailingPathDelimiter(GetEnvironmentFolder()), '', []),
      // Extracts to ./PYVER by default
      '.\assets\internal',
      false, true,
      TDeployOperation.doCopyOnly, //It seems Android doesn't support doUnArchive :(
      '')
  ];

  Result := Assigned(LFiles);
  Result.Args.SetFiles(LFiles);
end;

end.
