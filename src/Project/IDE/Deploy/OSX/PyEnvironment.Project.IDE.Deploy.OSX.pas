(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Deploy.macOS'                 *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality:  Make deployables for macOS                            *)
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
unit PyEnvironment.Project.IDE.Deploy.OSX;

interface

uses
  System.SysUtils,
  System.Classes,
  ToolsAPI,
  DeploymentAPI,
  PyEnvironment.Project.IDE.Types,
  PyEnvironment.Project.IDE.Deploy.Platform;

type
  // https://developer.apple.com/documentation/bundleresources/placing_content_in_a_bundle
  // Apple docs says that we need to place dynlibs onto Contents/Frameworks
  // It seems to work fine as is, but this is something to think about...

  TPyEnvironmentProjectDeployOSX = class(TPyEnvironmentProjectDeployPlatform)
  private
    function GetFrameworksFolder(const APythonVersion: string): string; inline;
    function GetExistingFrameworks: TArray<string>;
  private
    /// <summary>
    /// Create the frameworks template plist for the current project.
    /// </summary>
    function MakeFrameworkTemplatePlist: string;
    /// <summary>
    /// Create a framework for a single .dylib including a Info.plist for code sign.
    /// </summary>
    function MakeFramework(const ALibraryName: string;
      const ACopyFileProc: TProc<TStream>): TArray<string>;
    /// <summary>
    /// Opens the Python distribution .zip collecting the .dylib files
    /// to make frameworks.
    /// </summary>
    function MakeFrameworksFromZip: TArray<string>;
    /// <summary>
    /// Delete all frameworks folders.
    /// </summary>
    procedure ClearFrameworks;
    /// <summary>
    /// The minimal Python bundle to be distributed to OSX.
    /// Excludes many unnecessary files.
    /// </summary>
    function MakePythonMinimalBundle: string;
  protected
    function GetPythonBundleName: string; override;
    function GetBundleMinimalIgnoresList: TArray<string>; override;
    function Build: TArray<TPyEnvironmentDeployFile>; override;
  end;

const
  INFO_PLIST_TEMPLATE_NAME = 'info.plist.framework.TemplateOSX.xml';

  FRAMEWORK_TEMPLATE_PLIST =
    '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' + sLineBreak +
    '<plist version="1.0">' + sLineBreak +
    '<dict>' + sLineBreak +
    '  <key>CFBundleDevelopmentRegion</key>' + sLineBreak +
    '  <string>English</string>' + sLineBreak +
    '  <key>CFBundleExecutable</key>' + sLineBreak +
    '  <string>%CFBundleExecutable%</string>' + sLineBreak +
    '  <key>CFBundleIdentifier</key>' + sLineBreak +
    '  <string>%CFBundleIdentifier%</string>' + sLineBreak +
    '  <key>CFBundleInfoDictionaryVersion</key>' + sLineBreak +
    '  <string>6.0</string>' + sLineBreak +
    '  <key>CFBundlePackageType</key>' + sLineBreak +
    '  <string>FMWK</string>' + sLineBreak +
    '  <key>CFBundleSignature</key>' + sLineBreak +
    '  <string>????</string>' + sLineBreak +
    '  <key>CFBundleVersion</key>' + sLineBreak +
    '  <string>1</string>' + sLineBreak +
    '  <key>CFBundleShortVersionString</key>' + sLineBreak +
    '  <string>1</string>' + sLineBreak +
    '  <key>CSResourcesFileMapped</key>' + sLineBreak +
    '  <true/>' + sLineBreak +
    '</dict>' + sLineBreak +
    '</plist>';

implementation

uses
  System.StrUtils,
  System.IOUtils,
  System.Masks,
  System.Zip,
  PyEnvironment.Project.IDE.Deploy;

{ TPyEnvironmentProjectDeployOSX }

procedure TPyEnvironmentProjectDeployOSX.ClearFrameworks;
begin
  for var LPythonVersion in TPyEnvironmentProjectDeploy.PYTHON_VERSIONS do begin
    var LFrameworksFolder := GetFrameworksFolder(LPythonVersion);
    if TDirectory.Exists(LFrameworksFolder) then
      TDirectory.Delete(LFrameworksFolder, true);
  end;
end;

function TPyEnvironmentProjectDeployOSX.GetBundleMinimalIgnoresList: TArray<string>;
begin
  Result := inherited + [
    // Remove the Python shared lib - this MUST be a framework
    'lib/*.dylib'
  ];
end;

function TPyEnvironmentProjectDeployOSX.GetExistingFrameworks: TArray<string>;
begin
  Result := TDirectory.GetFiles(GetFrameworksFolder(PythonVersion),
    TSearchOption.soAllDirectories,
    function(const AFileName: string; const SearchRec: TSearchRec): boolean
    begin
      Result := String(SearchRec.Name).EndsWith('.dylib') or String(SearchRec.Name).EndsWith('.plist');
    end);
end;

function TPyEnvironmentProjectDeployOSX.GetFrameworksFolder(
  const APythonVersion: string): string;
begin
  // Deploy frameworks to the project's build folder
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
    + 'Frameworks');
end;

function TPyEnvironmentProjectDeployOSX.GetPythonBundleName: string;
begin
  case IndexStr(PythonVersion, ['3.8', '3.9', '3.10', '3.11', '3.12']) of
    0: Result := 'python3-macos-3.8.18-universal2.zip';
    1: Result := 'python3-macos-3.9.18-universal2.zip';
    2: Result := 'python3-macos-3.10.13-universal2.zip';
    3: Result := 'python3-macos-3.11.6-universal2.zip';
    4: Result := 'python3-macos-3.12.0-universal2.zip';
    else
      Result := String.Empty;
  end;
end;

function TPyEnvironmentProjectDeployOSX.MakeFramework(
  const ALibraryName: string;
  const ACopyFileProc: TProc<TStream>): TArray<string>;
begin
  // The bundle indentifier is base upon its base name.
  var LBundleIdentifier := IfThen(ALibraryName.Contains('.'), ALibraryName.Split(['.'])[0], ALibraryName);
  // The framework folder name.
  var LFrameworkName := LBundleIdentifier + '.framework';
  // The framework folder located in the project root folder.
  var LFrameworkFolder := TPath.Combine(GetFrameworksFolder(PythonVersion), LFrameworkName);
  // The framework library name; this is the distributed .dylib file located
  // in the framework folder.
  var LLibraryName := TPath.Combine(LFrameworkFolder, ALibraryName);
  // Frameworks requires plist. This is the template plist.
  var LInfoPlistTemplateFileName := MakeFrameworkTemplatePlist();
  // The framework plist. It goes alongside .dylib
  var LInfoPlistFileName := TPath.Combine(LFrameworkFolder, 'Info.plist');

  // Create the framework directory
  TDirectory.CreateDirectory(LFrameworkFolder);

  // Copy the framework dynamic library to the framework folder
  var LStream := TFileStream.Create(LLibraryName, fmCreate);
  try
    ACopyFileProc(LStream);
  finally
    LStream.Free;
  end;

  // Loads the Info.plist template
  { TODO : Must rebuild info.plist with project's info }
  var LBuff := TFile.ReadAllText(LInfoPlistTemplateFileName); // raise exception if template doesn't exist
  LBuff := LBuff
    .Replace('%CFBundleExecutable%', ALibraryName, [])
    .Replace('%CFBundleIdentifier%', Format('%s.%s', [
      TPath.GetFileNameWithoutExtension(GetProjectName()),
      LBundleIdentifier]), []);
  // Creates the framework Info.plist file
  TFile.WriteAllText(LInfoPlistFileName, LBuff, TEncoding.UTF8);

  // This is the framework files
  Result := [LLibraryName, LInfoPlistFileName];
end;

function TPyEnvironmentProjectDeployOSX.MakeFrameworksFromZip: TArray<string>;
begin
  if TDirectory.Exists(GetFrameworksFolder(PythonVersion)) then
    Exit(GetExistingFrameworks());

  // It is the first time set or something has changed. Let's clean up.
  ClearFrameworks();

  var LZip := TZipFile.Create;
  try
    LZip.Open(LocatePythonBundle(), TZipMode.zmRead);

    var LAssetFileName := Format('libpython%s.dylib', [PythonVersion]);

    Result := Result + MakeFramework(TPath.GetFileName(LAssetFileName),
      procedure(AStream: TStream)
      var
        LStream: TStream;
        LLocalHeader: TZipHeader;
      begin
        LZip.Read('lib/' + LAssetFileName, LStream, LLocalHeader);
        AStream.CopyFrom(LStream, LStream.Size);
      end);

    LZip.Close;
  finally
    LZip.Free;
  end;
end;

function TPyEnvironmentProjectDeployOSX.MakeFrameworkTemplatePlist: string;
begin
  Result := TPath.Combine(GetProjectFolder(), INFO_PLIST_TEMPLATE_NAME);
  if not TFile.Exists(Result) then
    TFile.WriteAllText(Result, FRAMEWORK_TEMPLATE_PLIST, TEncoding.UTF8);
end;

function TPyEnvironmentProjectDeployOSX.MakePythonMinimalBundle: string;

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

function TPyEnvironmentProjectDeployOSX.Build: TArray<TPyEnvironmentDeployFile>;
begin
  // Add frameworks to the deploy file list
  for var LFramework in MakeFrameworksFromZip() do
  begin
    var LDeployOp := TDeployOperation.doCopyOnly;
    var LDirs := LFramework.Split(TPath.DirectorySeparatorChar);
    var LFrameworkName := LDirs[High(LDirs) - 1];

    if LFramework.EndsWith('.dylib') then
      LDeployOp := TDeployOperation.doSetExecBit;

    Result := Result + [
      TPyEnvironmentDeployFile.Create(GetPlatform(),
        // Make the framework path relative to project's path
        LFramework.Replace(IncludeTrailingPathDelimiter(GetProjectFolder), '', []),
        'Contents\Frameworks\' + LFrameworkName,
        true,  true, LDeployOp, '', false)
    ];
  end;

  // Add the python minimal bundle to the deploy file list
  var LPythonLibZip := MakePythonMinimalBundle();
  Result := Result + [
    TPyEnvironmentDeployFile.Create(GetPlatform(),
      LPythonLibZip.Replace(IncludeTrailingPathDelimiter(PythonEnvironmentFolder), '', []),
      // Extracts to ./PYVER by default
      'Contents\Resources\' + PythonVersion,
      false,  true, TDeployOperation.doUnArchive, '')
  ];
end;

end.
