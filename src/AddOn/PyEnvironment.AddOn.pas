(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.AddOn'                                    *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(* Functionality: PyEnvironment AddOn                                     *)
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
unit PyEnvironment.AddOn;

interface

uses
  System.Classes, System.Generics.Collections, System.SysUtils,
  PyEnvironment,
  PyEnvironment.Notification,
  PyEnvironment.Distribution;

type
  TPyEnvironmentCustomAddOn = class;
//  TPyEnvironmentAddOns = class;

  TPyEnvironmentaddOnTrigger = (
    trBeforeSetup, trAfterSetup,
    trBeforeActivate, trAfterActivate,
    trBeforeDeactivate, trAfterDeactivate);
  TPyEnvironmentaddOnTriggers = set of TPyEnvironmentaddOnTrigger;

  TPyEnvironmentAddOnExecute = procedure(const ASender: TObject;
    const ATrigger: TPyEnvironmentaddOnTrigger;
    const ADistribution: TPyDistribution) of object;

//  TPyEnvironmentAddOnExecuteError = procedure(ADistribution: TPyDistribution;
//    const AAddOn: TPyEnvironmentCustomAddOn;
//    AException: Exception) of object;

  TPyEnvironmentAddOnExecuteError = procedure(const ASender: TObject;
    const ADistribution: TPyDistribution; const AException: Exception) of object;

  TPyEnvironmentCustomAddOn = class(TComponent, IEnvironmentNotified<TPyCustomEnvironment>)
  private
//    FAddOns: TPyEnvironmentAddOns;
    FEnvironment: TPyCustomEnvironment;
    FOnExecute: TPyEnvironmentAddOnExecute;
    FTriggers: TPyEnvironmentaddOnTriggers;
    FOnExecuteError: TPyEnvironmentAddOnExecuteError;
    procedure SetEnvironment(const Value: TPyCustomEnvironment);
//    procedure SetAddOns(const Value: TPyEnvironmentAddOns);
    //IEnvironmentNotified implementation
    procedure NotifyUpdate(const ANotifier: TPyCustomEnvironment;
      const ANotification: TEnvironmentNotification;
      const AArgs: TObject);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  protected
    //Getters and Setters
    procedure SetTriggers(const Value: TPyEnvironmentaddOnTriggers); virtual;
  protected
    function GetTriggerFromNotification(const ANotification: TEnvironmentNotification;
      out ATrigger: TPyEnvironmentaddOnTrigger): boolean; overload;
    function CanExecute(ATrigger: TPyEnvironmentaddOnTrigger): boolean; overload;


    procedure InternalExecute(const ATriggeredBy: TPyEnvironmentaddOnTrigger;
      const ADistribution: TPyDistribution); virtual; abstract;
  public
    procedure Execute(const ATrigger: TPyEnvironmentaddOnTrigger; const ADistribution: TPyDistribution);
  published
//    property AddOns: TPyEnvironmentAddOns read FAddOns write SetAddOns;
    property Environment: TPyCustomEnvironment read FEnvironment write SetEnvironment;
    property Triggers: TPyEnvironmentaddOnTriggers read FTriggers write SetTriggers;
    property OnExecute: TPyEnvironmentAddOnExecute read FOnExecute write FOnExecute;
    property OnExecuteError: TPyEnvironmentAddOnExecuteError read FOnExecuteError write FOnExecuteError;
  end;

  [ComponentPlatforms(pidAllPlatforms)]
  TPyEnvironmentAddOn = class(TPyEnvironmentCustomAddOn);

implementation

{ TPyEnvironmentCustomAddOn }

procedure TPyEnvironmentCustomAddOn.SetTriggers(
  const Value: TPyEnvironmentaddOnTriggers);
begin
  FTriggers := Value;
end;

procedure TPyEnvironmentCustomAddOn.SetEnvironment(
  const Value: TPyCustomEnvironment);
begin
  if Assigned(FEnvironment) then begin
    FEnvironment.RemoveFreeNotification(Self);
    (FEnvironment as IEnvironmentNotifier<TPyCustomEnvironment>).RemoveListener(Self);
  end;

  FEnvironment := Value;
  if Assigned(FEnvironment) then begin
    FEnvironment.FreeNotification(Self);
    (FEnvironment as IEnvironmentNotifier<TPyCustomEnvironment>).AddListener(Self);
  end;
end;

procedure TPyEnvironmentCustomAddOn.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FEnvironment) then begin
    SetEnvironment(nil);
  end;
end;

procedure TPyEnvironmentCustomAddOn.NotifyUpdate(
  const ANotifier: TPyCustomEnvironment;
  const ANotification: TEnvironmentNotification; const AArgs: TObject);
var
  LTrigger: TPyEnvironmentaddOnTrigger;
begin
  if GetTriggerFromNotification(ANotification, LTrigger) then
    Execute(LTrigger, (AArgs as TPyDistribution));
end;

function TPyEnvironmentCustomAddOn.GetTriggerFromNotification(
  const ANotification: TEnvironmentNotification;
  out ATrigger: TPyEnvironmentaddOnTrigger): boolean;
begin
  case ANotification of
    BEFORE_SETUP_NOTIFICATION:
      ATrigger := TPyEnvironmentaddOnTrigger.trBeforeSetup;
    AFTER_SETUP_NOTIFICATION:
      ATrigger := TPyEnvironmentaddOnTrigger.trAfterSetup;
    BEFORE_ACTIVATE_NOTIFICATION:
      ATrigger := TPyEnvironmentaddOnTrigger.trBeforeActivate;
    AFTER_ACTIVATE_NOTIFICATION:
      ATrigger := TPyEnvironmentaddOnTrigger.trAfterActivate;
    BEFORE_DEACTIVATE_NOTIFICATION:
      ATrigger := TPyEnvironmentaddOnTrigger.trBeforeDeactivate;
    AFTER_DEACTIVATE_NOTIFICATION:
      ATrigger := TPyEnvironmentaddOnTrigger.trAfterDeactivate;
    else
      Exit(false);
  end;
  Result := True;
end;

function TPyEnvironmentCustomAddOn.CanExecute(
  ATrigger: TPyEnvironmentaddOnTrigger): boolean;
begin
  Result := (ATrigger in Triggers);
end;

procedure TPyEnvironmentCustomAddOn.Execute(
  const ATrigger: TPyEnvironmentaddOnTrigger;
  const ADistribution: TPyDistribution);
begin
  if not CanExecute(ATrigger) then
    Exit;

  if Assigned(FOnExecute) then
    FOnExecute(Self, ATrigger, ADistribution);

  try
    InternalExecute(ATrigger, ADistribution);
  except
    on E: Exception do begin
      if Assigned(FOnExecuteError) then
        FOnExecuteError(Self, ADistribution, E)
      else
        raise;
    end;
  end;
end;

end.
