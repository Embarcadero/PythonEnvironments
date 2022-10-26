(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.AddOn.GetPip'                             *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality:  PyEnvironment GetPip add-on                           *)
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
unit PyEnvironment.AddOn.GetPip;

interface

uses
  System.SysUtils,
  System.Classes,
  PyTools.Cancelation,
  PyEnvironment,
  PyEnvironment.AddOn;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TPyEnvironmentAddOnGetPip = class(TPyEnvironmentCustomAddOn)
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

  EPipSetupFailed = class(Exception);

implementation

uses
  System.Types,
  System.IOUtils,
  PythonEngine,
  PyTools.ExecCmd,
  PyTools.ExecCmd.Args;

{$R ..\..\resources\getpipscript.res}

{ TPyEnvironmentAddOnGetPip }

procedure TPyEnvironmentAddOnGetPip.SetTriggers(
  const Value: TPyEnvironmentaddOnTriggers);
begin
  inherited SetTriggers([TPyEnvironmentaddOnTrigger.trAfterSetup]);
end;

constructor TPyEnvironmentAddOnGetPip.Create(AOwner: TComponent);
begin
  SetTriggers([TPyEnvironmentaddOnTrigger.trAfterSetup]);
  inherited;
end;

function TPyEnvironmentAddOnGetPip.GetInfo: TPyPluginInfo;
begin
  Result.Name := 'get-pip.py';
  Result.Description :=
    'Python script that uses some bootstrapping logic to install pip.'
    + sLineBreak
    + 'See more: https://pip.pypa.io/en/stable/installation/#get-pip-py';
  Result.InstallsWhen := [TPyPluginEvent.AfterActivate];
end;

function TPyEnvironmentAddOnGetPip.IsInstalled: boolean;
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

  // Check if pip is installed
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

procedure TPyEnvironmentAddOnGetPip.InternalExecute(
  const ACancelation: ICancelation);
var
  LPythonHome: string;
  LExecutable: string;
  LSharedLibrary: string;
  LResStream: TResourceStream;
  LFileName: string;
  LPths: TArray<string>;
  LStrings: TStringList;
  I: Integer;
  LOutput: string;
  LCmd: IExecCmd;
  LExitCode: Integer;
begin
  inherited;
  LPythonHome := GetPythonEngine().PythonHome;
  LExecutable := GetPythonEngine().ProgramName;
  LSharedLibrary := TPath.Combine(GetPythonEngine().DllPath,
    GetPythonEngine().DllName);

  LPths := TDirectory.GetFiles(
    LPythonHome, 'python*._pth', TSearchOption.soTopDirectoryOnly);
  if (Length(LPths) > 0) then begin
    LStrings := TStringList.Create();
    try
      LStrings.LoadFromFile(LPths[0]);
      for I := 0 to LStrings.Count -1 do
        if LStrings[I].Trim().StartsWith('#import site') then
          LStrings[I] := 'import site';
     LStrings.SaveToFile(LPths[0]);
    finally
      LStrings.Free();
    end;
  end;

  ACancelation.CheckCanceled();

  //Run the get-pip.py script to enabled PIP
  LFileName := TPath.GetTempFileName();
  LResStream := TResourceStream.Create(HInstance, 'getpippy', RT_RCDATA);
  try
    LResStream.SaveToFile(LFileName);
    LCmd := TExecCmdService.Cmd(
      LExecutable,
      TExecCmdArgs.BuildArgv(
        LExecutable, [LFileName]),
      TExecCmdArgs.BuildEnvp(
        LPythonHome,
        LExecutable,
        LSharedLibrary))
      .Run(LOutput);

    LExitCode := LCmd.SpinWait(function(): boolean begin
      Result := ACancelation.IsCanceled;
    end);

    if ACancelation.IsCanceled then
      LCmd.Kill()
    else if (LExitCode <> EXIT_SUCCESS) then
      raise EPipSetupFailed.Create('PIP setup has failed.' + #13#10 + LOutput);
  finally
    LResStream.Free();
  end;
end;

end.

