(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.AddOn.GetPip'                             *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
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
  System.SysUtils, System.Classes,
  PyEnvironment.Distribution, PyEnvironment.Notification, PyEnvironment.AddOn;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TPyEnvironmentAddOnGetPip = class(TPyEnvironmentCustomAddOn)
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
  System.Types, System.IOUtils, System.Variants,
  VarPyth,
  PyTools.ExecCmd, PyTools.ExecCmd.Args;

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

procedure TPyEnvironmentAddOnGetPip.InternalExecute(
  const ATriggeredBy: TPyEnvironmentaddOnTrigger;
  const ADistribution: TPyDistribution);
var
  LResStream: TResourceStream;
  LFileName: string;
  LPths: TArray<string>;
  LStrings: TStringList;
  I: Integer;
  LOut: string;
begin
  inherited;
  
  //Patch the _pth file to work with site packages
  LPths := TDirectory.GetFiles(
    ADistribution.Home, 'python*._pth', TSearchOption.soTopDirectoryOnly);
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

  //Run the get-pip.py script to enabled PIP
  LFileName := TPath.GetTempFileName();
  LResStream := TResourceStream.Create(HInstance, 'getpippy', RT_RCDATA);
  try
    LResStream.SaveToFile(LFileName);
    if TExecCmdService.Cmd(ADistribution.Executable,
         TExecCmdArgs.BuildArgv(
           ADistribution.Executable, [LFileName]),
         TExecCmdArgs.BuildEnvp(
           ADistribution.Home,
           ADistribution.Executable,
           ADistribution.SharedLibrary))
       .Run(LOut)
         .Wait() <> EXIT_SUCCESS then
           raise EPipSetupFailed.Create('Failed to setup PIP.' + #13#10 + LOut);
  finally
    LResStream.Free();
  end;
end;

end.

