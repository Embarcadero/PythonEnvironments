(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Helper'                       *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality: Helpers for the Python project menu                    *)
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
unit PyEnvironment.Project.IDE.Helper;

interface

uses
  System.Classes,
  ToolsAPI, DeploymentAPI,
  PyEnvironment.Project.IDE.Types;

type
  TPyEnvironmentProjectHelper = record
  strict private
    const
      PYTHON_VERSION_PREFIX = 'PYTHONVER';
    class function ContainsStringInArray(const AString: string; const AArray: TArray<string>): Boolean; static; inline;
    class function GetIsPyEnvironmentDefined(
      const AProject: IOTAProject): Boolean; static;
    class procedure SetIsPyEnvironmentDefined(const AProject: IOTAProject;
      const AValue: Boolean); static;
    class function GetCurrentPythonVersion(
      const AProject: IOTAProject): string; static;
    class procedure SetCurrentPythonVersion(const AProject: IOTAProject;
      const AValue: string); static;
    class function BuildPythonVersionConditional(const APythonVersion: string): string; static;
  public
    class procedure AddDeployFile(const AProject: IOTAProject; const AConfig: TPyEnvironmentProjectConfig; const ADeployFile: TPyEnvironmentDeployFile); static;
    class procedure RemoveDeployFile(const AProject: IOTAProject; const AConfig: TPyEnvironmentProjectConfig; const APlatform: TPyEnvironmentProjectPlatform; ALocalFileName: string; const ARemoteDir: string); static;
    class procedure RemoveDeployFilesOfClass(const AProject: IOTAProject); overload; static;
    class procedure RemoveDeployFilesOfClass(const AProject: IOTAProject; const AConfig: TPyEnvironmentProjectConfig; const APlatform: TPyEnvironmentProjectPlatform); overload; static;
    class procedure RemoveUnexpectedDeployFilesOfClass(const AProject: IOTAProject; const AConfig: TPyEnvironmentProjectConfig; const APlatform: TPyEnvironmentProjectPlatform; const AAllowedFiles: TArray<TPyEnvironmentDeployFile>); static;

    class function IsPyEnvironmentDefinedForPlatform(const AProject: IOTAProject; const APlatform: TPyEnvironmentProjectPlatform; const AConfig: TPyEnvironmentProjectConfig): Boolean; static;
    class function SupportsEnvironmentDeployment(const AProject: IOTAProject): Boolean; static;

    class property IsPyEnvironmentDefined[const AProject: IOTAProject]: Boolean read GetIsPyEnvironmentDefined write SetIsPyEnvironmentDefined;
    class property CurrentPythonVersion[const AProject: IOTAProject]: string read GetCurrentPythonVersion write SetCurrentPythonVersion;
  end;

  TPyEnvironmentOTAHelper = record
  strict private
    const
      DefaultOptionsSeparator = ';';
      OutputDirPropertyName = 'OutputDir';
    class function ExpandConfiguration(const ASource: string; const AConfig: IOTABuildConfiguration): string; static;
    class function ExpandEnvironmentVar(var AValue: string): Boolean; static;
    class function ExpandOutputPath(const ASource: string; const ABuildConfig: IOTABuildConfiguration): string; static;
    class function ExpandPath(const ABaseDir, ARelativeDir: string): string; static;
    class function ExpandVars(const ASource: string): string; static;
    class function GetEnvironmentVars(const AVars: TStrings; AExpand: Boolean): Boolean; static;
    class function GetProjectOptionsConfigurations(const AProject: IOTAProject): IOTAProjectOptionsConfigurations; static;
    class procedure MultiSzToStrings(const ADest: TStrings; const ASource: PChar); static;
    class procedure StrResetLength(var S: string); static;
    class function TryGetProjectOutputPath(const AProject: IOTAProject; ABuildConfig: IOTABuildConfiguration; out AOutputPath: string): Boolean; overload; static;
  public
    class function ContainsOptionValue(const AValues, AValue: string; const ASeparator: string = DefaultOptionsSeparator): Boolean; static;
    class function InsertOptionValue(const AValues, AValue: string; const ASeparator: string = DefaultOptionsSeparator): string; static;
    class function RemoveOptionValue(const AValues, AValue: string; const ASeparator: string = DefaultOptionsSeparator): string; static;

    class function GetEnvironmentVar(const AName: string; AExpand: Boolean): string; static;
    class function TryCopyFileToOutputPath(const AProject: IOTAProject; const APlatform: TPyEnvironmentProjectPlatform; const AConfig: TPyEnvironmentProjectConfig; const AFileName: string): Boolean; static;
    class function TryCopyFileToOutputPathOfActiveBuild(const AProject: IOTAProject; const AFileName: string): Boolean; static;
    class function TryGetBuildConfig(const AProject: IOTAProject; const APlatform: TPyEnvironmentProjectPlatform; const AConfig: TPyEnvironmentProjectConfig; out ABuildConfig: IOTABuildConfiguration): Boolean; static;
    class function TryGetProjectOutputPath(const AProject: IOTAProject; const APlatform: TPyEnvironmentProjectPlatform; const AConfig: TPyEnvironmentProjectConfig; out AOutputPath: string): Boolean; overload; static;
    class function TryGetProjectOutputPathOfActiveBuild(const AProject: IOTAProject; out AOutputPath: string): Boolean; static;
    class function TryRemoveOutputFile(const AProject: IOTAProject; const APlatform: TPyEnvironmentProjectPlatform; const AConfig: TPyEnvironmentProjectConfig; AFileName: string): Boolean; static;
    class function TryRemoveOutputFileOfActiveBuild(const AProject: IOTAProject; const AFileName: string): Boolean; static;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Generics.Collections,
  DccStrs,
  Winapi.Windows, Winapi.ShLwApi,
  PyEnvironment.Project.IDE.Deploy;

{ TPyEnvironmentProjectHelper }

class procedure TPyEnvironmentProjectHelper.AddDeployFile(
  const AProject: IOTAProject; const AConfig: TPyEnvironmentProjectConfig;
  const ADeployFile: TPyEnvironmentDeployFile);
type
  TDeployFileExistence = (DoesNotExist, AlreadyExists, NeedReplaced);

  function GetDeployFileExistence(const AProjectDeployment: IProjectDeployment;
    const ALocalFileName, ARemoteDir, APlatformName, AConfigName: string): TDeployFileExistence;
  var
    LRemoteFileName: string;
    LFile: IProjectDeploymentFile;
    LFiles: TDictionary<string, IProjectDeploymentFile>.TValueCollection;
  begin
    Result := TDeployFileExistence.DoesNotExist;
    LRemoteFileName := TPath.Combine(ARemoteDir, TPath.GetFileName(ALocalFileName));
    LFiles := AProjectDeployment.Files;
    if Assigned(LFiles) then begin
      for LFile in LFiles do begin
        if (LFile.FilePlatform = APlatformName) and (LFile.Configuration = AConfigName) then begin
          if SameText(LRemoteFileName, TPath.Combine(LFile.RemoteDir[APlatformName], LFile.RemoteName[APlatformName])) then begin
            if (LFile.LocalName = ALocalFileName) and (LFile.DeploymentClass = TPyEnvironmentProjectDeploy.DEPLOYMENT_CLASS) and
              (LFile.Condition = ADeployFile.Condition) and (LFile.Operation[APlatformName] = ADeployFile.Operation) and
              LFile.Enabled[APlatformName] and LFile.Overwrite[APlatformName] and
              (LFile.Required = ADeployFile.Required) and (Result = TDeployFileExistence.DoesNotExist) then
            begin
              Result := TDeployFileExistence.AlreadyExists;
            end else
              Exit(TDeployFileExistence.NeedReplaced);
          end;
        end;
      end;
    end;
  end;

  function CanDeployFile(const ADeployFileExistence: TDeployFileExistence): boolean;
  begin
    Result := (ADeployFileExistence in [TDeployFileExistence.NeedReplaced,
      TDeployFileExistence.DoesNotExist])
      and (AConfig in ADeployFile.Configs)
  end;

  procedure DoAddDeployFile(const AProjectDeployment: IProjectDeployment;
    const ALocalFileName, APlatformName, AConfigName: string);
  var
    LFile: IProjectDeploymentFile;
  begin
    LFile := AProjectDeployment.CreateFile(AConfigName, APlatformName, ALocalFileName);
    if Assigned(LFile) then begin
      LFile.Overwrite[APlatformName] := True;
      LFile.Enabled[APlatformName] := True;
      LFile.Required := ADeployFile.Required;
      LFile.Condition := ADeployFile.Condition;
      LFile.Operation[APlatformName] := ADeployFile.Operation;
      LFile.RemoteDir[APlatformName] := ADeployFile.RemotePath;
      LFile.DeploymentClass := TPyEnvironmentProjectDeploy.DEPLOYMENT_CLASS;
      LFile.RemoteName[APlatformName] := TPath.GetFileName(ALocalFileName);
      AProjectDeployment.AddFile(AConfigName, APlatformName, LFile);
    end;
  end;

var
  LProjectDeployment: IProjectDeployment;
  LConfigName: string;
  LPlatformName: string;
  LLocalFileName: string;
  LDeployFileExistence: TDeployFileExistence;
begin
  if (ADeployFile.LocalFileName <> '') and Supports(AProject, IProjectDeployment, LProjectDeployment)  then begin
    LConfigName := AConfig.ToString;
    LPlatformName := ADeployFile.Platform.ToString;
    LLocalFileName := TPath.Combine(TPyEnvironmentProjectDeploy.Path, ADeployFile.LocalFileName);
    LDeployFileExistence := GetDeployFileExistence(LProjectDeployment, LLocalFileName, ADeployFile.RemotePath, LPlatformName, LConfigName);
    if LDeployFileExistence = TDeployFileExistence.NeedReplaced then
      RemoveDeployFile(AProject, AConfig, ADeployFile.Platform, ADeployFile.LocalFileName, ADeployFile.RemotePath);
    if CanDeployFile(LDeployFileExistence) then
      DoAddDeployFile(LProjectDeployment, LLocalFileName, LPlatformName, LConfigName);
  end;
end;

class function TPyEnvironmentProjectHelper.BuildPythonVersionConditional(
  const APythonVersion: string): string;
begin
  Result := PYTHON_VERSION_PREFIX + APythonVersion.Replace('.', '', [rfReplaceAll]);
end;

class function TPyEnvironmentProjectHelper.ContainsStringInArray(
  const AString: string; const AArray: TArray<string>): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(AArray) to High(AArray) do
    if AArray[I] = AString then
      Exit(True);
end;

class function TPyEnvironmentProjectHelper.GetCurrentPythonVersion(
  const AProject: IOTAProject): string;
var
  LBaseConfiguration: IOTABuildConfiguration;
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
  LPythonVersion: string;
begin
  if not (Assigned(AProject)
    and Supports(AProject.ProjectOptions, IOTAProjectOptionsConfigurations, LOptionsConfigurations)
      and Assigned(LOptionsConfigurations.BaseConfiguration)) then
        Exit(String.Empty);

  LBaseConfiguration := LOptionsConfigurations.BaseConfiguration;
  if Assigned(LBaseConfiguration) then begin
    for LPythonVersion in TPyEnvironmentProjectDeploy.PYTHON_VERSIONS do begin
      if TPyEnvironmentOTAHelper.ContainsOptionValue(
        LBaseConfiguration.Value[sDefine],
          BuildPythonVersionConditional(LPythonVersion)) then
            Exit(LPythonVersion);
    end;
  end;

  Result := String.Empty;
end;

class function TPyEnvironmentProjectHelper.GetIsPyEnvironmentDefined(
  const AProject: IOTAProject): Boolean;
var
  LBaseConfiguration: IOTABuildConfiguration;
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
begin
  Result := Assigned(AProject)
    and Supports(AProject.ProjectOptions, IOTAProjectOptionsConfigurations, LOptionsConfigurations);

  if Result then begin
    LBaseConfiguration := LOptionsConfigurations.BaseConfiguration;
    Result := Assigned(LBaseConfiguration)
      and TPyEnvironmentOTAHelper.ContainsOptionValue(
        LBaseConfiguration.Value[sDefine], TPyEnvironmentProjectDeploy.PROJECT_USE_PYTHON);
  end;
end;

class function TPyEnvironmentProjectHelper.IsPyEnvironmentDefinedForPlatform(
  const AProject: IOTAProject; const APlatform: TPyEnvironmentProjectPlatform;
  const AConfig: TPyEnvironmentProjectConfig): Boolean;
var
  LBuildConfig: IOTABuildConfiguration;
begin
  Assert(IsPyEnvironmentDefined[AProject]);
  Result := TPyEnvironmentOTAHelper.TryGetBuildConfig(
    AProject, APlatform, AConfig, LBuildConfig)
      and not TPyEnvironmentOTAHelper.ContainsOptionValue(
        LBuildConfig.Value[sDefine], TPyEnvironmentProjectDeploy.PROJECT_NO_USE_PYTHON);
end;

class procedure TPyEnvironmentProjectHelper.RemoveDeployFile(
  const AProject: IOTAProject; const AConfig: TPyEnvironmentProjectConfig;
  const APlatform: TPyEnvironmentProjectPlatform; ALocalFileName: string;
  const ARemoteDir: string);
var
  LProjectDeployment: IProjectDeployment;
  LFiles: TDictionary<string, IProjectDeploymentFile>.TValueCollection;
  LFile: IProjectDeploymentFile;
  LRemoteFileName: string;
  LRemoveFiles: TArray<IProjectDeploymentFile>;
begin
  if (ALocalFileName <> '') and Supports(AProject, IProjectDeployment, LProjectDeployment) then begin
    ALocalFileName := TPath.Combine(TPyEnvironmentProjectDeploy.Path, ALocalFileName);
    LProjectDeployment.RemoveFile(AConfig.ToString, APlatform.ToString, ALocalFileName);
    LFiles := LProjectDeployment.Files;
    if Assigned(LFiles) then begin
      LRemoteFileName := TPath.Combine(ARemoteDir, TPath.GetFileName(ALocalFileName));
      LRemoveFiles := [];
      for LFile in LFiles do
        if SameText(LRemoteFileName, TPath.Combine(LFile.RemoteDir[APlatform.ToString], LFile.RemoteName[APlatform.ToString])) then
          LRemoveFiles := LRemoveFiles + [LFile];
      for LFile in LRemoveFiles do
        LProjectDeployment.RemoveFile(AConfig.ToString, APlatform.ToString, LFile.LocalName);
    end;
  end;
end;

class procedure TPyEnvironmentProjectHelper.RemoveDeployFilesOfClass(
  const AProject: IOTAProject; const AConfig: TPyEnvironmentProjectConfig;
  const APlatform: TPyEnvironmentProjectPlatform);
var
  LProjectDeployment: IProjectDeployment;
  LFile: IProjectDeploymentFile;
  LConfigName: string;
  LPlatformName: string;
begin
  if Supports(AProject, IProjectDeployment, LProjectDeployment) then begin
    LConfigName := AConfig.ToString;
    LPlatformName := APlatform.ToString;
    for LFile in LProjectDeployment.GetFilesOfClass(TPyEnvironmentProjectDeploy.DEPLOYMENT_CLASS) do
      if (LFile.Configuration = LConfigName) and ContainsStringInArray(LPlatformName, LFile.Platforms) then
        LProjectDeployment.RemoveFile(LConfigName, LPlatformName, LFile.LocalName);
  end;
end;

class procedure TPyEnvironmentProjectHelper.RemoveUnexpectedDeployFilesOfClass(
  const AProject: IOTAProject; const AConfig: TPyEnvironmentProjectConfig;
  const APlatform: TPyEnvironmentProjectPlatform;
  const AAllowedFiles: TArray<TPyEnvironmentDeployFile>);

  function IsAllowedFile(const AFile: IProjectDeploymentFile; const APlatformName: string): Boolean;
  var
    LDeployFile: TPyEnvironmentDeployFile;
  begin
    Result := False;
    for LDeployFile in AAllowedFiles do begin
      if (AFile.LocalName = LDeployFile.LocalFileName) and SameText(AFile.RemoteDir[APlatformName], LDeployFile.RemotePath) and
        SameText(AFile.RemoteName[APlatformName], TPath.GetFileName(LDeployFile.LocalFileName)) and
        (AFile.DeploymentClass = TPyEnvironmentProjectDeploy.DEPLOYMENT_CLASS) and
        (AFile.Condition = LDeployFile.Condition) and (AFile.Operation[APlatformName] = LDeployFile.Operation) and
        AFile.Enabled[APlatformName] and AFile.Overwrite[APlatformName] and
        (AFile.Required = LDeployFile.Required) then
      begin
        Exit(True);
      end;
    end;
  end;

var
  LProjectDeployment: IProjectDeployment;
  LFile: IProjectDeploymentFile;
  LConfigName: string;
  LPlatformName: string;
begin
  if Supports(AProject, IProjectDeployment, LProjectDeployment) then begin
    LConfigName := AConfig.ToString;
    LPlatformName := APlatform.ToString;
    for LFile in LProjectDeployment.GetFilesOfClass(TPyEnvironmentProjectDeploy.DEPLOYMENT_CLASS) do begin
      if (LFile.Configuration = LConfigName) and ContainsStringInArray(LPlatformName, LFile.Platforms) and
        not IsAllowedFile(LFile, LPlatformName) then
      begin
        LProjectDeployment.RemoveFile(LConfigName, LPlatformName, LFile.LocalName);
      end;
    end;
  end;
end;

class procedure TPyEnvironmentProjectHelper.RemoveDeployFilesOfClass(
  const AProject: IOTAProject);
var
  LProjectDeployment: IProjectDeployment;
begin
  if Supports(AProject, IProjectDeployment, LProjectDeployment) then
    LProjectDeployment.RemoveFilesOfClass(TPyEnvironmentProjectDeploy.DEPLOYMENT_CLASS);
end;

class procedure TPyEnvironmentProjectHelper.SetCurrentPythonVersion(
  const AProject: IOTAProject; const AValue: string);
var
  LProjectOptions: IOTAProjectOptions;
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
  LBaseConfiguration: IOTABuildConfiguration;
  LPythonVersion: string;
begin
  if Assigned(AProject) then begin
    LProjectOptions := AProject.ProjectOptions;
    if Assigned(LProjectOptions) then begin
      if Supports(LProjectOptions, IOTAProjectOptionsConfigurations, LOptionsConfigurations) then begin
        LBaseConfiguration := LOptionsConfigurations.BaseConfiguration;
        if Assigned(LBaseConfiguration) then begin
          LBaseConfiguration := LOptionsConfigurations.BaseConfiguration;
          for LPythonVersion in TPyEnvironmentProjectDeploy.PYTHON_VERSIONS do
            LBaseConfiguration.Value[sDefine] :=
              TPyEnvironmentOTAHelper.RemoveOptionValue(
                LBaseConfiguration.Value[sDefine],
                BuildPythonVersionConditional(LPythonVersion));

          if not AValue.IsEmpty() then
            LBaseConfiguration.Value[sDefine] := TPyEnvironmentOTAHelper
              .InsertOptionValue(LBaseConfiguration.Value[sDefine],
                BuildPythonVersionConditional(AValue));
        end;
      end;
      LProjectOptions.ModifiedState := True;
    end;
  end;
end;

class procedure TPyEnvironmentProjectHelper.SetIsPyEnvironmentDefined(
  const AProject: IOTAProject; const AValue: Boolean);
var
  LProjectOptions: IOTAProjectOptions;
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
  LBaseConfiguration: IOTABuildConfiguration;
begin
  if Assigned(AProject) then begin
    LProjectOptions := AProject.ProjectOptions;
    if Assigned(LProjectOptions) then begin
      if Supports(LProjectOptions, IOTAProjectOptionsConfigurations, LOptionsConfigurations) then begin
        LBaseConfiguration := LOptionsConfigurations.BaseConfiguration;
        if Assigned(LBaseConfiguration) then begin
          if AValue then
            LBaseConfiguration.Value[sDefine] := TPyEnvironmentOTAHelper
              .InsertOptionValue(
                LBaseConfiguration.Value[sDefine], TPyEnvironmentProjectDeploy.PROJECT_USE_PYTHON)
          else
            LBaseConfiguration.Value[sDefine] := TPyEnvironmentOTAHelper
              .RemoveOptionValue(
                LBaseConfiguration.Value[sDefine], TPyEnvironmentProjectDeploy.PROJECT_USE_PYTHON);
        end;
      end;
      LProjectOptions.ModifiedState := True;
    end;
  end;
end;

class function TPyEnvironmentProjectHelper.SupportsEnvironmentDeployment(
  const AProject: IOTAProject): Boolean;
begin
  Result := Assigned(AProject)
    and AProject.FileName.EndsWith('.dproj', True)
    and ((AProject.ApplicationType = sApplication)
        or
        (AProject.ApplicationType = sConsole));
end;

{ TPyEnvironmentOTAHelper }

class function TPyEnvironmentOTAHelper.ContainsOptionValue(const AValues,
  AValue, ASeparator: string): Boolean;
var
  LValues: TArray<string>;
  I: Integer;
begin
  LValues := AValues.Split([ASeparator], TStringSplitOptions.None);
  for I := 0 to Length(LValues) - 1 do
    if SameText(LValues[I], AValue) then
      Exit(True);
  Result := False;
end;

class function TPyEnvironmentOTAHelper.ExpandConfiguration(
  const ASource: string; const AConfig: IOTABuildConfiguration): string;
begin
  Result := StringReplace(ASource, '$(Platform)', AConfig.Platform, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$(Config)', AConfig.Name, [rfReplaceAll, rfIgnoreCase]);
end;

class function TPyEnvironmentOTAHelper.ExpandEnvironmentVar(
  var AValue: string): Boolean;
var
  R: Integer;
  LExpanded: string;
begin
  SetLength(LExpanded, 1);
  R := ExpandEnvironmentStrings(PChar(AValue), PChar(LExpanded), 0);
  SetLength(LExpanded, R);
  Result := ExpandEnvironmentStrings(PChar(AValue), PChar(LExpanded), R) <> 0;
  if Result then begin
    StrResetLength(LExpanded);
    AValue := LExpanded;
  end;
end;

class function TPyEnvironmentOTAHelper.ExpandOutputPath(const ASource: string;
  const ABuildConfig: IOTABuildConfiguration): string;
begin
  if Assigned(ABuildConfig) then
    Result := ExpandConfiguration(ASource, ABuildConfig)
  else
    Result := ASource;
  Result := ExpandVars(Result);
end;

class function TPyEnvironmentOTAHelper.ExpandPath(const ABaseDir,
  ARelativeDir: string): string;
var
  LBuffer: array [0..MAX_PATH - 1] of Char;
begin
  if PathIsRelative(PChar(ARelativeDir)) then
    Result := IncludeTrailingPathDelimiter(ABaseDir) + ARelativeDir
  else
    Result := ARelativeDir;
  if PathCanonicalize(@LBuffer[0], PChar(Result)) then
    Result := LBuffer;
end;

class function TPyEnvironmentOTAHelper.ExpandVars(
  const ASource: string): string;
var
  LVars: TStrings;
  I: Integer;
begin
  Result := ASource;
  if not Result.IsEmpty then begin
    LVars := TStringList.Create;
    try
      GetEnvironmentVars(LVars, True);
      for I := 0 to LVars.Count - 1 do begin
        Result := StringReplace(Result, '$(' + LVars.Names[I] + ')', LVars.Values[LVars.Names[I]], [rfReplaceAll, rfIgnoreCase]);
        Result := StringReplace(Result, '%' + LVars.Names[I] + '%', LVars.Values[LVars.Names[I]], [rfReplaceAll, rfIgnoreCase]);
      end;
    finally
      LVars.Free;
    end;
  end;
end;

class function TPyEnvironmentOTAHelper.GetEnvironmentVar(const AName: string;
  AExpand: Boolean): string;
const
  BufSize = 1024;
var
  Len: Integer;
  Buffer: array[0..BufSize - 1] of Char;
  LExpanded: string;
begin
  Result := '';
  Len := Winapi.Windows.GetEnvironmentVariable(PChar(AName), @Buffer, BufSize);
  if Len < BufSize then
    SetString(Result, PChar(@Buffer), Len)
  else begin
    SetLength(Result, Len - 1);
    Winapi.Windows.GetEnvironmentVariable(PChar(AName), PChar(Result), Len);
  end;
  if AExpand then begin
    LExpanded := Result;
    if ExpandEnvironmentVar(LExpanded) then
      Result := LExpanded;
  end;
end;

class function TPyEnvironmentOTAHelper.GetEnvironmentVars(const AVars: TStrings;
  AExpand: Boolean): Boolean;
var
  LRaw: PChar;
  LExpanded: string;
  I: Integer;
begin
  AVars.BeginUpdate;
  try
    AVars.Clear;
    LRaw := GetEnvironmentStrings;
    try
      MultiSzToStrings(AVars, LRaw);
      Result := True;
    finally
      FreeEnvironmentStrings(LRaw);
    end;
    if AExpand then begin
      for I := 0 to AVars.Count - 1 do begin
        LExpanded := AVars[I];
        if ExpandEnvironmentVar(LExpanded) then
          AVars[I] := LExpanded;
      end;
    end;
  finally
    AVars.EndUpdate;
  end;
end;

class function TPyEnvironmentOTAHelper.GetProjectOptionsConfigurations(
  const AProject: IOTAProject): IOTAProjectOptionsConfigurations;
var
  LProjectOptions: IOTAProjectOptions;
begin
  Result := nil;
  if AProject <> nil then begin
    LProjectOptions := AProject.ProjectOptions;
    if LProjectOptions <> nil then
      Supports(LProjectOptions, IOTAProjectOptionsConfigurations, Result);
  end;
end;

class function TPyEnvironmentOTAHelper.InsertOptionValue(const AValues, AValue,
  ASeparator: string): string;
var
  LValues: TArray<string>;
  I: Integer;
begin
  LValues := AValues.Split([ASeparator], TStringSplitOptions.None);
  try
    for I := 0 to Length(LValues) - 1 do begin
      if SameText(LValues[I], AValue) then begin
        LValues[I] := AValue;
        Exit;
      end;
    end;
    LValues := LValues + [AValue];
  finally
    if LValues = nil then
      Result := ''
    else
      Result := string.Join(ASeparator, LValues);
  end;
end;

class procedure TPyEnvironmentOTAHelper.MultiSzToStrings(const ADest: TStrings;
  const ASource: PChar);
var
  P: PChar;
begin
  ADest.BeginUpdate;
  try
    ADest.Clear;
    if ASource <> nil then begin
      P := ASource;
      while P^ <> #0 do begin
        ADest.Add(P);
        P := StrEnd(P);
        Inc(P);
      end;
    end;
  finally
    ADest.EndUpdate;
  end;
end;

class function TPyEnvironmentOTAHelper.RemoveOptionValue(const AValues, AValue,
  ASeparator: string): string;
var
  LValues: TArray<string>;
  LNewValues: TArray<string>;
  I: Integer;
begin
  LNewValues := [];
  LValues := AValues.Split([ASeparator], TStringSplitOptions.None);
  for I := 0 to Length(LValues) - 1 do
    if not SameText(LValues[I], AValue) then
      LNewValues := LNewValues + [LValues[I]];
  if LNewValues = nil then
    Result := ''
  else
    Result := string.Join(ASeparator, LNewValues);
end;

class procedure TPyEnvironmentOTAHelper.StrResetLength(var S: string);
begin
  SetLength(S, StrLen(PChar(S)));
end;

class function TPyEnvironmentOTAHelper.TryGetProjectOutputPath(
  const AProject: IOTAProject; ABuildConfig: IOTABuildConfiguration;
  out AOutputPath: string): Boolean;
var
  LOptions: IOTAProjectOptions;
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
  LRelativeOutputPath: string;
begin
  Result := False;
  try
    if Assigned(AProject) then begin
      AOutputPath := TPath.GetDirectoryName(AProject.FileName);
      LOptions := AProject.ProjectOptions;
      if LOptions <> nil then begin
        if not Assigned(ABuildConfig) then begin
          LOptionsConfigurations := GetProjectOptionsConfigurations(AProject);
          if Assigned(LOptionsConfigurations) then
            ABuildConfig := LOptionsConfigurations.ActiveConfiguration;
        end;

        if Assigned(ABuildConfig) then begin
          LRelativeOutputPath := LOptions.Values[OutputDirPropertyName];
          AOutputPath := ExpandOutputPath(ExpandPath(AOutputPath, LRelativeOutputPath), ABuildConfig);
          Result := True;
        end else
          Result := False;
      end else
        Result := True;
    end;
  finally
    if not Result then
      AOutputPath := '';
  end;
end;

class function TPyEnvironmentOTAHelper.TryCopyFileToOutputPath(
  const AProject: IOTAProject; const APlatform: TPyEnvironmentProjectPlatform;
  const AConfig: TPyEnvironmentProjectConfig; const AFileName: string): Boolean;
var
  LProjectOutputPath: string;
begin
  Result := False;
  if (APlatform <> TPyEnvironmentProjectPlatform.Unknown) and TFile.Exists(AFileName)
    and TryGetProjectOutputPath(AProject, APlatform, AConfig, LProjectOutputPath) then
  begin
    try
      if not TDirectory.Exists(LProjectOutputPath) then
        TDirectory.CreateDirectory(LProjectOutputPath);
      TFile.Copy(AFileName, TPath.Combine(LProjectOutputPath, TPath.GetFileName(AFileName)), True);
      Result := True;
    except
      Result := False;
    end;
  end;
end;

class function TPyEnvironmentOTAHelper.TryCopyFileToOutputPathOfActiveBuild(
  const AProject: IOTAProject; const AFileName: string): Boolean;
var
  LPlatform: TPyEnvironmentProjectPlatform;
  LConfig: TPyEnvironmentProjectConfig;
begin
  LPlatform := TPyEnvironmentProjectPlatform.Unknown;
  LConfig := TPyEnvironmentProjectConfig.Release;
  if Assigned(AProject) then begin
    LPlatform := TPyEnvironmentProjectPlatform.FromString(AProject.CurrentPlatform);
    LConfig := TPyEnvironmentProjectConfig.FromString(AProject.CurrentConfiguration);
  end;
  Result := TryCopyFileToOutputPath(AProject, LPlatform, LConfig, AFileName);
end;

class function TPyEnvironmentOTAHelper.TryGetBuildConfig(
  const AProject: IOTAProject; const APlatform: TPyEnvironmentProjectPlatform;
  const AConfig: TPyEnvironmentProjectConfig;
  out ABuildConfig: IOTABuildConfiguration): Boolean;
var
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
  LConfigName: string;
  I: Integer;
begin
  Result := False;
  ABuildConfig := nil;
  if APlatform <> TPyEnvironmentProjectPlatform.Unknown then begin
    LOptionsConfigurations := GetProjectOptionsConfigurations(AProject);
    if Assigned(LOptionsConfigurations) then begin
      LConfigName := AConfig.ToString;
      for I := LOptionsConfigurations.ConfigurationCount - 1 downto 0 do begin
        ABuildConfig := LOptionsConfigurations.Configurations[I];
        if ContainsOptionValue(ABuildConfig.Value[sDefine], LConfigName) then begin
          ABuildConfig := ABuildConfig.PlatformConfiguration[APlatform.ToString];
          Exit(Assigned(ABuildConfig));
        end;
      end;
    end;
  end;
end;

class function TPyEnvironmentOTAHelper.TryGetProjectOutputPath(
  const AProject: IOTAProject; const APlatform: TPyEnvironmentProjectPlatform;
  const AConfig: TPyEnvironmentProjectConfig; out AOutputPath: string): Boolean;
var
  LBuildConfig: IOTABuildConfiguration;
begin
  Result := (APlatform <> TPyEnvironmentProjectPlatform.Unknown) and
    TryGetBuildConfig(AProject, APlatform, AConfig, LBuildConfig) and
    TryGetProjectOutputPath(AProject, LBuildConfig, AOutputPath) and
    TPath.HasValidPathChars(AOutputPath, False);
  if not Result then
    AOutputPath := '';
end;

class function TPyEnvironmentOTAHelper.TryGetProjectOutputPathOfActiveBuild(
  const AProject: IOTAProject; out AOutputPath: string): Boolean;
var
  LPlatform: TPyEnvironmentProjectPlatform;
  LConfig: TPyEnvironmentProjectConfig;
begin
  LPlatform := TPyEnvironmentProjectPlatform.Unknown;
  LConfig := TPyEnvironmentProjectConfig.Release;
  if Assigned(AProject) then begin
    LPlatform := TPyEnvironmentProjectPlatform.FromString(AProject.CurrentPlatform);
    LConfig := TPyEnvironmentProjectConfig.FromString(AProject.CurrentConfiguration);
  end;
  Result := TryGetProjectOutputPath(AProject, LPlatform, LConfig, AOutputPath);
end;

class function TPyEnvironmentOTAHelper.TryRemoveOutputFile(
  const AProject: IOTAProject; const APlatform: TPyEnvironmentProjectPlatform;
  const AConfig: TPyEnvironmentProjectConfig; AFileName: string): Boolean;
var
  LProjectOutputPath: string;
begin
  Result := False;
  if (APlatform <> TPyEnvironmentProjectPlatform.Unknown) and TPyEnvironmentOTAHelper.TryGetProjectOutputPathOfActiveBuild(AProject, LProjectOutputPath) then begin
    AFileName := TPath.Combine(LProjectOutputPath, AFileName);
    if TFile.Exists(AFileName) then begin
      try
        TFile.Delete(AFileName);
        Result := True;
      except
        Result := False;
      end;
    end;
  end;
end;

class function TPyEnvironmentOTAHelper.TryRemoveOutputFileOfActiveBuild(
  const AProject: IOTAProject; const AFileName: string): Boolean;
var
  LPlatform: TPyEnvironmentProjectPlatform;
  LConfig: TPyEnvironmentProjectConfig;
begin
  LPlatform := TPyEnvironmentProjectPlatform.Unknown;
  LConfig := TPyEnvironmentProjectConfig.Release;
  if Assigned(AProject) then begin
    LPlatform := TPyEnvironmentProjectPlatform.FromString(AProject.CurrentPlatform);
    LConfig := TPyEnvironmentProjectConfig.FromString(AProject.CurrentConfiguration);
  end;
  Result := TryRemoveOutputFile(AProject, LPlatform, LConfig, AFileName);
end;

end.
