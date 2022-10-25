(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.AddOn.EnsurePip'                          *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality:  PyEnvironment EnsurePIP add-on                        *)
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
unit PyEnvironment.AddOn.EnsurePip;

interface

uses
  System.SysUtils, System.Classes,
  PyEnvironment.Distribution, PyEnvironment.Notification, PyEnvironment.AddOn;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TPyEnvironmentAddOnEnsurePip = class(TPyEnvironmentCustomAddOn)
  protected
    procedure SetTriggers(const Value: TPyEnvironmentaddOnTriggers); override;
    procedure InternalExecute(const ATriggeredBy: TPyEnvironmentaddOnTrigger;
      const ADistribution: TPyDistribution); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Triggers default [TPyEnvironmentaddOnTrigger.trAfterSetup];
  end;

  EPipSetupFailed = class(Exception);

implementation

uses
  PyTools.ExecCmd, PyTools.ExecCmd.Args;

{ TPyEnvironmentAddOnEnsurePip }

procedure TPyEnvironmentAddOnEnsurePip.SetTriggers(
  const Value: TPyEnvironmentaddOnTriggers);
begin
  inherited SetTriggers([TPyEnvironmentaddOnTrigger.trAfterSetup]);
end;

constructor TPyEnvironmentAddOnEnsurePip.Create(AOwner: TComponent);
begin
  SetTriggers([TPyEnvironmentaddOnTrigger.trAfterSetup]);
  inherited;
end;

procedure TPyEnvironmentAddOnEnsurePip.InternalExecute(
  const ATriggeredBy: TPyEnvironmentaddOnTrigger;
  const ADistribution: TPyDistribution);
var
  LOutput: string;
begin
  inherited;
  { TODO : Check for valida executable and shared library files }
  if (TExecCmdService.Cmd(ADistribution.Executable,
        TExecCmdArgs.BuildArgv(
          ADistribution.Executable, ['-m', 'pip', '--version']),
        TExecCmdArgs.BuildEnvp(
          ADistribution.Home,
          ADistribution.Executable,
          ADistribution.SharedLibrary)
      ).Run().Wait() = EXIT_SUCCESS) then
        Exit;

  if (TExecCmdService.Cmd(ADistribution.Executable,
        TExecCmdArgs.BuildArgv(
          ADistribution.Executable, ['-m', 'ensurepip']),
        TExecCmdArgs.BuildEnvp(
          ADistribution.Home,
          ADistribution.Executable,
          ADistribution.SharedLibrary)
      ).Run(LOutput).Wait() <> EXIT_SUCCESS) then
    raise EPipSetupFailed.Create('PIP setup has failed.' + #13#10 + LOutput);
end;

end.
