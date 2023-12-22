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
(*  Functionality: Python project menu types                              *)
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
unit PyEnvironment.Project.IDE.Types;

interface

uses
  System.Generics.Collections,
  DeploymentAPI;

type
  TPyEnvironmentProjectConfig = (Release, Debug);
  TPyEnvironmentProjectConfigs = set of TPyEnvironmentProjectConfig;
  TPyEnvironmentProjectPlatform = (Unknown, Win32, Win64, Android, Android64, iOSDevice32, iOSDevice64, iOSSimulator, iOSSimARM64, OSX64, OSXARM64, Linux64);

  TPyEvironmentProjectConfigHelper = record helper for TPyEnvironmentProjectConfig
    function ToString: string;
    class function FromString(const AText: string): TPyEnvironmentProjectConfig; static;
  end;

  TPyEvironmentProjectPlatformHelper = record helper for TPyEnvironmentProjectPlatform
    function ToString: string;
    class function FromString(const AText: string): TPyEnvironmentProjectPlatform; static;
  end;

  TPyEnvironmentDeployFile = record
    Configs: TPyEnvironmentProjectConfigs;
    &Platform: TPyEnvironmentProjectPlatform;
    LocalFileName: string;
    RemotePath: string;
    CopyToOutput: Boolean;
    Required: Boolean;
    Operation: TDeployOperation;
    Condition: string;
    UpdateLocalFileName: boolean;

    constructor Create(const AConfigs: TPyEnvironmentProjectConfigs;
      const APlatform: TPyEnvironmentProjectPlatform;
      const ALocalFileName, ARemotePath: string;
      const ACopyToOutput, ARequired: boolean;
      const AOperation: TDeployOperation; const ACondition: string;
      const AUpdateLocalFileName: boolean = true); overload;

    constructor Create(const APlatform: TPyEnvironmentProjectPlatform;
      const ALocalFileName, ARemotePath: string;
      const ACopyToOutput, ARequired: boolean;
      const AOperation: TDeployOperation; const ACondition: string;
      const AUpdateLocalFileName: boolean = true); overload;
  end;

implementation

uses
  TypInfo;

{ TPyEnvironmentDeployFile }

constructor TPyEnvironmentDeployFile.Create(
  const APlatform: TPyEnvironmentProjectPlatform; const ALocalFileName,
  ARemotePath: string; const ACopyToOutput, ARequired: boolean;
  const AOperation: TDeployOperation; const ACondition: string;
  const AUpdateLocalFileName: boolean);
begin
  Create([Release, Debug], APlatform, ALocalFileName, ARemotePath,
    ACopyToOutput, ARequired, AOperation, ACondition, AUpdateLocalFileName);
end;

constructor TPyEnvironmentDeployFile.Create(
  const AConfigs: TPyEnvironmentProjectConfigs;
  const APlatform: TPyEnvironmentProjectPlatform; const ALocalFileName,
  ARemotePath: string; const ACopyToOutput, ARequired: boolean;
  const AOperation: TDeployOperation; const ACondition: string;
  const AUpdateLocalFileName: boolean);
begin
  Configs := AConfigs;
  &Platform := APlatform;
  LocalFileName := ALocalFilename;
  RemotePath := ARemotePath;
  CopyToOutput := ACopyToOutput;
  Required := ARequired;
  Operation := AOperation;
  Condition := ACondition;
  UpdateLocalFileName := AUpdateLocalFileName;
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
