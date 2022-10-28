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
  System.SysUtils,
  System.Classes,
  PyTools.Cancelation,
  PyEnvironment,
  PyEnvironment.AddOn;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TPyEnvironmentAddOnEnsurePip = class(TPyEnvironmentCustomAddOn)
  protected
    function GetInfo(): TPyPluginInfo; override;
    function IsInstalled(): boolean; override;
    procedure SetTriggers(const Value: TPyEnvironmentaddOnTriggers); override;
    procedure InternalExecute(const ACancelation: ICancelation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Triggers default [TPyEnvironmentaddOnTrigger.trAfterSetup];
  end;

implementation

uses
  System.IOUtils,
  System.SyncObjs,
  PythonEngine,
  PyTools.ExecCmd,
  PyTools.ExecCmd.Args,
  PyEnvironment.Exception;

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

function TPyEnvironmentAddOnEnsurePip.GetInfo: TPyPluginInfo;
begin
  Result.Name := 'ensurepip';
  Result.Description :=
    'Provides support for bootstrapping the pip installer into an existing '
    + 'Python installation or virtual environment.'
    + sLineBreak
    + 'See more: https://pip.pypa.io/en/stable/installation/#ensurepip';
  Result.InstallsWhen := [TPyPluginEvent.AfterActivate];
end;

function TPyEnvironmentAddOnEnsurePip.IsInstalled: boolean;
var
  LPythonHome: string;
  LExecutable: string;
  LSharedLibrary: string;
  LCmd: IExecCmd;
begin
  LPythonHome := GetPythonEngine().PythonHome;
  LExecutable := GetPythonEngine().ProgramName;
  LSharedLibrary := TPath.Combine(GetPythonEngine().DllPath,
    GetPythonEngine().DllName);

  LCmd := TExecCmdService.Cmd(
    LExecutable,
    TExecCmdArgs.BuildArgv(
      LExecutable, ['-m', 'pip', '--version']),
    TExecCmdArgs.BuildEnvp(
      LPythonHome,
      LExecutable,
      LSharedLibrary)
    ).Run();

  Result := (LCmd.Wait() = EXIT_SUCCESS);
end;

procedure TPyEnvironmentAddOnEnsurePip.InternalExecute(
  const ACancelation: ICancelation);
var
  LPythonHome: string;
  LExecutable: string;
  LSharedLibrary: string;
  LCmd: IExecCmd;
begin
  inherited;
  LPythonHome := GetPythonEngine().PythonHome;
  LExecutable := GetPythonEngine().ProgramName;
  LSharedLibrary := TPath.Combine(GetPythonEngine().DllPath,
    GetPythonEngine().DllName);

  LCmd := TExecCmdService.Cmd(
    LExecutable,
    TExecCmdArgs.BuildArgv(
      LExecutable, ['-m', 'ensurepip']),
    TExecCmdArgs.BuildEnvp(
      LPythonHome,
      LExecutable,
      LSharedLibrary))
    .Run([TRedirect.stderr]);

  TSpinWait.SpinUntil(function(): boolean begin
    Result := not LCmd.IsAlive or ACancelation.IsCancelled;
  end);

  ACancelation.CheckCancelled();

  if (LCmd.Wait() <> EXIT_SUCCESS) then
    raise EPipSetupFailed.Create('PIP setup has failed.' + #13#10 + LCmd.StdErr.ReadAll());
end;

end.
