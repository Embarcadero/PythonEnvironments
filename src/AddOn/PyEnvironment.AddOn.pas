(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.AddOn'                                    *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
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
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Types,
  System.Rtti,
  System.Threading,
  PyTools.Cancelation,
  PyEnvironment;

type
  TPyEnvironmentCustomAddOn = class;

  TPyEnvironmentaddOnTrigger = (
    trBeforeSetup, trAfterSetup,
    trBeforeActivate, trAfterActivate,
    trBeforeDeactivate, trAfterDeactivate);

  TPyEnvironmentaddOnTriggers = set of TPyEnvironmentaddOnTrigger;

  TPyEnvironmentAddOnExecute = procedure(const ASender: TObject) of object;

  TPyEnvironmentAddOnExecuteError = procedure(const ASender: TObject;
    const AException: Exception) of object;

  TPyEnvironmentCustomAddOn = class(TComponent, IPyEnvironmentPlugin)
  private type
    TPyEnvironmentCustomAddonAsyncResult = class(TBaseAsyncResult)
    private
      FAsyncTask: TProc<ICancelation>;
      FCancelation: ICancelation;
    protected
      procedure AsyncDispatch; override;
      procedure Schedule; override;
      function DoCancel: Boolean; override;
    public
      constructor Create(const AContext: TObject; const AAsyncTask: TProc<ICancelation>);
    end;
  private
    FEnvironment: TPyCustomEnvironment;
    FOnExecute: TPyEnvironmentAddOnExecute;
    FTriggers: TPyEnvironmentaddOnTriggers;
    FOnExecuteError: TPyEnvironmentAddOnExecuteError;
    procedure SetEnvironment(const Value: TPyCustomEnvironment);
    //IPlugin implementation
    function Install(const AArgs: TArray<TValue> = []): IAsyncResult;
    function Uninstall(const AArgs: TArray<TValue> = []): IAsyncResult;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  protected
    //Getters and Setters
    function GetInfo(): TPyPluginInfo; virtual; abstract;
    procedure SetTriggers(const Value: TPyEnvironmentaddOnTriggers); virtual;
  protected
    procedure InternalExecute(const ACancelation: ICancelation); virtual; abstract;
    function IsInstalled(): boolean; virtual; abstract;
  public
    destructor Destroy(); override;

    procedure Execute(const ACancelation: ICancelation);
  published
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
    if not (csDesigning in ComponentState) then
      FEnvironment.RemovePlugion(Self);
  end;

  FEnvironment := Value;
  if Assigned(FEnvironment) then begin
    FEnvironment.FreeNotification(Self);
    if not (csDesigning in ComponentState) then
      FEnvironment.AddPlugin(Self);
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

destructor TPyEnvironmentCustomAddOn.Destroy;
begin
  SetEnvironment(nil);
  inherited;
end;

procedure TPyEnvironmentCustomAddOn.Execute(const ACancelation: ICancelation);
begin
  Assert(Assigned(ACancelation), 'Invalid argument "ACancelation".');

  if Assigned(FOnExecute) then
    FOnExecute(Self);

  try
    InternalExecute(ACancelation);
  except
    on E: Exception do begin
      if Assigned(FOnExecuteError) then
        FOnExecuteError(Self, E)
      else
        raise;
    end;
  end;
end;

function TPyEnvironmentCustomAddOn.Install(
  const AArgs: TArray<TValue>): IAsyncResult;
begin
  Result := TPyEnvironmentCustomAddonAsyncResult.Create(Self,
    procedure(ACancelation: ICancelation) begin
      Execute(ACancelation);
    end).Invoke();
end;

function TPyEnvironmentCustomAddOn.Uninstall(
  const AArgs: TArray<TValue>): IAsyncResult;
begin
  //
end;

{ TPyEnvironmentCustomAddOn.TPyEnvironmentCustomAddonAsyncResult }

constructor TPyEnvironmentCustomAddOn.TPyEnvironmentCustomAddonAsyncResult.Create(
  const AContext: TObject; const AAsyncTask: TProc<ICancelation>);
begin
  inherited Create(AContext);
  FAsyncTask := AAsyncTask;
  FCancelation := TCancelation.Create();
end;

function TPyEnvironmentCustomAddOn.TPyEnvironmentCustomAddonAsyncResult.DoCancel: Boolean;
begin
  FCancelation.Cancel();
  Result := true;
end;

procedure TPyEnvironmentCustomAddOn.TPyEnvironmentCustomAddonAsyncResult.AsyncDispatch;
begin
  FAsyncTask(FCancelation);
end;

procedure TPyEnvironmentCustomAddOn.TPyEnvironmentCustomAddonAsyncResult.Schedule;
begin
  TTask.Run(DoAsyncDispatch);
end;

end.
