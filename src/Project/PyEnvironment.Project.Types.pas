unit PyEnvironment.Project.Types;

interface

uses
  DeploymentAPI;

type
  TPyEnvironmentProjectConfig = (Release, Debug);
  TPyEnvironmentProjectPlatform = (Unknown, Win32, Win64, Android, Android64, iOSDevice32, iOSDevice64, iOSSimulator, OSX64, OSXARM64, Linux64);

  TPyEvironmentProjectConfigHelper = record helper for TPyEnvironmentProjectConfig
    function ToString: string;
    class function FromString(const AText: string): TPyEnvironmentProjectConfig; static;
  end;

  TPyEvironmentProjectPlatformHelper = record helper for TPyEnvironmentProjectPlatform
    function ToString: string;
    class function FromString(const AText: string): TPyEnvironmentProjectPlatform; static;
  end;

  TPyEnvironmentDeployFile = record
    &Platform: TPyEnvironmentProjectPlatform;
    LocalFileName: string;
    RemotePath: string;
    CopyToOutput: Boolean;
    Required: Boolean;
    Operation: TDeployOperation;
    Condition: string;

    constructor Create(const APlatform: TPyEnvironmentProjectPlatform;
      const ALocalFileName, ARemotePath: string;
      const ACopyToOutput, ARequired: boolean;
      const AOperation: TDeployOperation; const ACondition: string);
  end;

implementation

uses
  TypInfo;

{ TPyEnvironmentDeployFile }

constructor TPyEnvironmentDeployFile.Create(
  const APlatform: TPyEnvironmentProjectPlatform; const ALocalFileName,
  ARemotePath: string; const ACopyToOutput, ARequired: boolean;
  const AOperation: TDeployOperation; const ACondition: string);
begin
  &Platform := APlatform;
  LocalFileName := ALocalFilename;
  RemotePath := ARemotePath;
  CopyToOutput := ACopyToOutput;
  Required := ARequired;
  Operation := AOperation;
  Condition := ACondition;
end;

{ TPyEvironmentProjectConfigHelper }

class function TPyEvironmentProjectConfigHelper.FromString(
  const AText: string): TPyEnvironmentProjectConfig;
begin
  Result := TPyEnvironmentProjectConfig(GetEnumValue(TypeInfo(TPyEnvironmentProjectConfig), AText));
end;

function TPyEvironmentProjectConfigHelper.ToString: string;
begin
    Result := GetEnumName(TypeInfo(TPyEnvironmentProjectConfig), Ord(Self));
end;

{ TPyEvironmentProjectPlatformHelper }

class function TPyEvironmentProjectPlatformHelper.FromString(
  const AText: string): TPyEnvironmentProjectPlatform;
var
  LEnumValue: Integer;
begin
  LEnumValue := GetEnumValue(TypeInfo(TPyEnvironmentProjectPlatform), AText);
  if LEnumValue = -1 then
    Result := TPyEnvironmentProjectPlatform.Unknown
  else
    Result := TPyEnvironmentProjectPlatform(GetEnumValue(TypeInfo(TPyEnvironmentProjectPlatform), AText));
end;

function TPyEvironmentProjectPlatformHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TPyEnvironmentProjectPlatform), Ord(Self));
end;

end.
