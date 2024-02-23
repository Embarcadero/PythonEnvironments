(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Deploy.iOS'                   *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality:  Make deployables for iOS                              *)
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
unit PyEnvironment.Project.IDE.Deploy.iOS;

interface

uses
  System.SysUtils,
  System.Classes,
  ToolsAPI,
  DeploymentAPI,
  PyEnvironment.Project.IDE.Types,
  PyEnvironment.Project.IDE.Deploy.Platform;

type
  TPyEnvironmentProjectDeployIOS = class(TPyEnvironmentProjectDeployPlatform)
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
  protected
    function GetBundleMinimalIgnoresList: TArray<string>; override;
  protected
    function Make(const AInput: TDeployTaskInput): TDeployTaskOutput; override;
    function Deploy(const AInput: TDeployTaskInput): TDeployTaskOutput; override;
  end;

implementation

uses
  System.Masks,
  System.IOUtils,
  System.StrUtils,
  System.Zip,
  PyEnvironment.Project.IDE.Helper,
  PyEnvironment.Project.IDE.Deploy;

const
  INFO_PLIST_TEMPLATE_NAME = 'info.plist.framework.TemplateiOS.xml';

  FRAMEWORK_TEMPLATE_PLIST =
    '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' + sLineBreak +
    '<plist version="1.0">' + sLineBreak +
    '<dict>' + sLineBreak +
    '  <key>CFBundleDevelopmentRegion</key>' + sLineBreak +
    '  <string>en</string>' + sLineBreak +
    '  <key>CFBundleExecutable</key>' + sLineBreak +
    '  <string>%CFBundleExecutable%</string>' + sLineBreak +
    '  <key>CFBundleIdentifier</key>' + sLineBreak +
    '  <string>%CFBundleIdentifier%</string>' + sLineBreak +
    '  <key>CFBundleInfoDictionaryVersion</key>' + sLineBreak +
    '  <string>6.0</string>' + sLineBreak +
    '  <key>CFBundlePackageType</key>' + sLineBreak +
    '  <string>APPL</string>' + sLineBreak +
    '  <key>CFBundleShortVersionString</key>' + sLineBreak +
    '  <string>1.0</string>' + sLineBreak +
    '  <key>CFBundleSupportedPlatforms</key>' + sLineBreak +
    '  <array>' + sLineBreak +
    '    <string>iPhoneOS</string>' + sLineBreak +
    '  </array>' + sLineBreak +
    '  <key>MinimumOSVersion</key>' + sLineBreak +
    '  <string>12.0</string>' + sLineBreak +
    '  <key>CFBundleVersion</key>' + sLineBreak +
    '  <string>1</string>' + sLineBreak +
    '</dict>' + sLineBreak +
    '</plist>';

{ TPyEnvironmentProjectDeployIOS }

function TPyEnvironmentProjectDeployIOS.GetFrameworksFolder(
  const APythonVersion: string): string;
begin
  // Deploy frameworks to the project's build folder
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
    + 'Frameworks');
end;

function TPyEnvironmentProjectDeployIOS.GetBundleMinimalIgnoresList: TArray<string>;
begin
  Result := inherited + [
    // We can't create a Python sub process.
    'bin/*',
    // Remove the Python shared lib - this MUST be a framework
    'lib/*.dylib',
    // Remove all shared libs
    'lib/python3.*/lib-dynload/*'
  ];
end;

function TPyEnvironmentProjectDeployIOS.GetExistingFrameworks: TArray<string>;
begin
  Result := TDirectory.GetFiles(GetFrameworksFolder(GetPythonVersion()),
    TSearchOption.soAllDirectories,
    function(const AFileName: string; const SearchRec: TSearchRec): boolean
    begin
      Result := String(SearchRec.Name).EndsWith('.dylib') or String(SearchRec.Name).EndsWith('.plist');
    end);
end;

function TPyEnvironmentProjectDeployIOS.MakeFrameworkTemplatePlist: string;
begin
  Result := TPath.Combine(GetProjectFolder(), INFO_PLIST_TEMPLATE_NAME);
  if not TFile.Exists(Result) then
    TFile.WriteAllText(Result, FRAMEWORK_TEMPLATE_PLIST, TEncoding.UTF8);
end;

function TPyEnvironmentProjectDeployIOS.MakeFramework(
  const ALibraryName: string;
  const ACopyFileProc: TProc<TStream>): TArray<string>;
begin
  // The bundle indentifier is base upon its base name.
  var LBundleIdentifier := IfThen(ALibraryName.Contains('.'), ALibraryName.Split(['.'])[0], ALibraryName);
  // The framework folder name.
  var LFrameworkName := LBundleIdentifier + '.framework';
  // The framework folder located in the project root folder.
  var LFrameworkFolder := TPath.Combine(GetFrameworksFolder(GetPythonVersion()), LFrameworkName);
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

function TPyEnvironmentProjectDeployIOS.MakeFrameworksFromZip: TArray<string>;
begin
  if TDirectory.Exists(GetFrameworksFolder(GetPythonVersion())) then
    Exit(GetExistingFrameworks());

  // It is the first time set or something has changed. Let's clean up.
  ClearFrameworks();

  var LZip := TZipFile.Create;
  try
    LZip.Open(LocatePythonBundle(), TZipMode.zmRead);
    for var LFileName in LZip.FileNames do
    begin
      // We MUST generate frameworks for .dylib files
      if not LFileName.EndsWith('.dylib') then
        Continue;
      // Remove test binary modules
      if TPath.GetFileName(LFileName).Contains('_test')
         or
         TPath.GetFileName(LFileName).StartsWith('_xx')
         or
         TPath.GetFileName(LFileName).StartsWith('xx') then
            Continue;

      Result := Result + MakeFramework(TPath.GetFileName(LFileName),
        procedure(AStream: TStream)
        var
          LStream: TStream;
          LLocalHeader: TZipHeader;
        begin
          LZip.Read(LFileName, LStream, LLocalHeader);
          AStream.CopyFrom(LStream, LStream.Size);
        end);
    end;
    LZip.Close;
  finally
    LZip.Free;
  end;
end;

procedure TPyEnvironmentProjectDeployIOS.ClearFrameworks;
begin
  for var LPythonVersion in TPyEnvironmentProjectDeploy.PYTHON_VERSIONS do begin
    var LFrameworksFolder := GetFrameworksFolder(LPythonVersion);
    if TDirectory.Exists(LFrameworksFolder) then
      TDirectory.Delete(LFrameworksFolder, true);
  end;
end;

function TPyEnvironmentProjectDeployIOS.Make(
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
        // The minimal bundle does't have the /lib/pythonVER/ folder
        var LFileName := String(AFileName)
          .Replace('lib/', '', [])
          .Replace('python' + GetPythonVersion() + '/', '', []);

        if not LFileName.IsEmpty then
          LZipWriter.Add(
            AData,
            LFileName);
      end);
    LZipWriter.Close;
  finally
    LZipWriter.Free;
  end;

  Result := TFile.Exists(LFileName);
end;

function TPyEnvironmentProjectDeployIOS.Deploy(
  const AInput: TDeployTaskInput): TDeployTaskOutput;
var
  LFileName: string;
  LFiles: TPyEnvironmentDeployFiles;
begin
  LFileName := GetBundleMinimalFileName();
  if not TFile.Exists(LFileName) then
    Exit(false);

  LFiles := [];

  // Add frameworks to the deploy file list
  for var LFramework in MakeFrameworksFromZip() do
  begin
    var LDeployOp := TDeployOperation.doCopyOnly;
    var LDirs := LFramework.Split(TPath.DirectorySeparatorChar);
    var LFrameworkName := LDirs[High(LDirs) - 1];

    if LFramework.EndsWith('.dylib') then
      LDeployOp := TDeployOperation.doSetExecBit;

    LFiles := LFiles + [
      TPyEnvironmentDeployFile.Create(GetPlatform(),
        // Make the framework path relative to project's path
        LFramework.Replace(IncludeTrailingPathDelimiter(GetProjectFolder), '', []),
        '.\Frameworks\' + LFrameworkName,
        true,  true, LDeployOp, '', false)
    ];
  end;

  // Add the python minimal bundle to the deploy file list
  LFiles := LFiles + [
    TPyEnvironmentDeployFile.Create(GetPlatform(),
      LFileName.Replace(IncludeTrailingPathDelimiter(GetEnvironmentFolder()), '', []),
      // Extracts to ./PYVER by default
      '.\' + GetPythonVersion(),
      false,  true, TDeployOperation.doUnArchive, '')
  ];

  Result := Assigned(LFiles);
  Result.Args.SetFiles(LFiles);
end;

end.
