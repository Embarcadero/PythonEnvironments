(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Local'                                    *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyEnvironment Local                                   *)
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
unit PyEnvironment.Local;

interface

uses
  System.Classes, System.SysUtils, System.JSON,
  PyEnvironment, PyEnvironment.Distribution;

type
  (*-----------------------------------------------------------------------*)
  (*                                                                       *)
  (*                         JSON structure example                        *)
  (*                                                                       *)
  (* JSON                                                                  *)
  (*    [{"python_version":                                                *)
  (*         {"home": "",                                                  *)
  (*          "shared_library": "",                                        *)
  (*          "executable": ""}}]                                          *)
  (*-----------------------------------------------------------------------*)
  TPyLocalDistribution = class(TPyDistribution)
  public
    procedure Setup(); override;
  end;

  TPyLocalCollection = class(TPyDistributionCollection);

  [ComponentPlatforms(pidAllPlatforms)]
  TPyLocalEnvironment = class(TPyEnvironment)
  private
    FFilePath: string;
    procedure EnumerateEnvironments(const AProc: TProc<string, TJSONObject>);
  protected
    function CreateCollection(): TPyDistributionCollection; override;
    procedure Prepare(); override;
  published
    property Distributions;
    property FilePath: string read FFilePath write FFilePath;
  end;

  EInvalidFileStructure = class(Exception);

implementation

uses
  System.IOUtils, PythonEngine, PyEnvironment.Path;

{ TPyLocalDistribution }

procedure TPyLocalDistribution.Setup;
begin
  inherited;
end;

{ TPyLocalEnvironment }

function TPyLocalEnvironment.CreateCollection: TPyDistributionCollection;
begin
  Result := TPyLocalCollection.Create(Self, TPyLocalDistribution);
end;

procedure TPyLocalEnvironment.EnumerateEnvironments(const AProc: TProc<string, TJSONObject>);
var
  LPythonVersions: TJSONValue;
  I: Integer;
  LPythonVersion: TJSONValue;
  LDistribution: TJSONValue;
  LFilePath: string;
begin
  LFilePath := TPyEnvironmentPath.ResolvePath(FFilePath);
  if not TFile.Exists(LFilePath) then
    raise EFileNotFoundException.CreateFmt('File not found.' + #13#10 + '%s', [LFilePath]);

  LPythonVersions := TJSONObject.ParseJSONValue(TFile.ReadAllText(LFilePath));
  try
    if not (Assigned(LPythonVersions) and (LPythonVersions is TJSONArray)) then
      raise EInvalidFileStructure.Create('Invalid file structure.');

    for I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do begin
      for LPythonVersion in TJSONArray(LPythonVersions) do begin
        if not (LPythonVersion is TJSONObject) then
          raise EInvalidFileStructure.Create('Invalid file structure.');

        LDistribution := TJSONObject(LPythonVersion).Values[PYTHON_KNOWN_VERSIONS[I].RegVersion];

        if not Assigned(LDistribution) then
          Continue;

        if not (LDistribution is TJSONObject) then
          raise EInvalidFileStructure.Create('Invalid file structure.');

        AProc(PYTHON_KNOWN_VERSIONS[I].RegVersion, TJSONObject(LDistribution));
      end;
    end;
  finally
    LPythonVersions.Free();
  end;
end;

procedure TPyLocalEnvironment.Prepare;
var
  LFilePath: string;
begin
  LFilePath := TPyEnvironmentPath.ResolvePath(FFilePath);
  if not TFile.Exists(LFilePath) then
    raise Exception.Create('File not found.');

  EnumerateEnvironments(
    procedure(APythonVersion: string; AEnvironmentInfo: TJSONObject)
    var
      LDistribution: TPyLocalDistribution;
    begin
      LDistribution := TPyLocalDistribution(Distributions.Add());
      LDistribution.PythonVersion := APythonVersion;
      LDistribution.Home := AEnvironmentInfo.GetValue<string>('home');
      LDistribution.SharedLibrary := AEnvironmentInfo.GetValue<string>('shared_library');
      LDistribution.Executable := AEnvironmentInfo.GetValue<string>('executable');
    end);

  inherited;
end;

end.
