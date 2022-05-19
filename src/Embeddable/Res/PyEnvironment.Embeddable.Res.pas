(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Embeddable.Res'                           *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyEnvironment Embeddable as a Resource                *)
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
unit PyEnvironment.Embeddable.Res;

interface

uses
  System.Classes,
  PyEnvironment.Distribution,
  PyEnvironment.Embeddable;

type
  TPyCustomEmbeddableResDistribution = class(TPyCustomEmbeddableDistribution)
  end;

  TPyEmbeddableCustomResCollection = class(TPyEmbeddableCustomCollection);

  TPyCustomEmbeddedResEnvironment = class(TPyCustomEmbeddedEnvironment)
  private
    FEnvironmentPath: string;
  protected
    function CreateCollection(): TPyDistributionCollection; override;
    procedure Prepare(); override;

    function GetResName(): string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property EnvironmentPath: string read FEnvironmentPath write FEnvironmentPath;
  end;

implementation

uses
  System.SysUtils, System.Types, System.IOUtils, System.Character,
  PyENvironment.Path;

{ TPyCustomEmbeddedResEnvironment }

constructor TPyCustomEmbeddedResEnvironment.Create(AOwner: TComponent);
begin
  SetPythonVersion(String.Empty);
  inherited;
end;

function TPyCustomEmbeddedResEnvironment.CreateCollection: TPyDistributionCollection;
begin
  Result := TPyEmbeddableCustomResCollection.Create(Self, TPyCustomEmbeddableResDistribution);
end;

function TPyCustomEmbeddedResEnvironment.GetResName: string;
var
  I: Integer;
begin
  Result := String.Empty;
  for I := Low(PythonVersion) to High(PythonVersion) do
    if Char.IsDigit(PythonVersion, I - 1) then
      Result := Result + PythonVersion[I];
  Result := 'python' + Result;
end;

procedure TPyCustomEmbeddedResEnvironment.Prepare;
var
  LResName: string;
  LEmbeddablePath: string;
  LResStream: TResourceStream;
  LDistribution: TPyCustomEmbeddableResDistribution;
begin
  LResName := GetResName();
  LEmbeddablePath := TPath.Combine(TPath.GetTempPath(), LResName);
  LResStream := TResourceStream.Create(HInstance, LResName, RT_RCDATA);
  try
    LResStream.SaveToFile(LEmbeddablePath);

    LDistribution := TPyCustomEmbeddableResDistribution(Distributions.Add());
    LDistribution.PythonVersion := PythonVersion;
    LDistribution.EnvironmentPath := TPath.Combine(
      TPyEnvironmentPath.ResolvePath(EnvironmentPath),
      PythonVersion);
    LDistribution.EmbeddablePackage := LEmbeddablePath;
    LDistribution.OnZipProgress := OnZipProgress;
  finally
    LResStream.Free();
  end;
  inherited;
end;

end.
