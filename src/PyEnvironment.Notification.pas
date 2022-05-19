(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironments.Notification'                            *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyEnvironments Notification layer                     *)
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
unit PyEnvironment.Notification;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TEnvironmentNotification = byte;

  IEnvironmentNotified<Notifier> = interface
    ['{D528B694-C4C4-4A96-BF40-2153CCC80243}']
    procedure NotifyUpdate(const ANotifier: Notifier;
      const ANotification: TEnvironmentNotification;
      const AArgs: TObject);
  end;

  IEnvironmentNotifier<Notifier> = interface
    ['{6214AB2E-4BD6-472E-B3A1-B6FCB46B1798}']
    procedure AddListener(const AListener: IEnvironmentNotified<Notifier>);
    procedure RemoveListener(const AListener: IEnvironmentNotified<Notifier>);

    procedure NotifyAll(const ANotification: TEnvironmentNotification;
      const AArgs: TObject);
  end;

  TEnvironmentBroadcaster<Notifier> = class(TInterfacedObject, IEnvironmentNotifier<Notifier>)
  private
    FNotifier: Notifier;
    FListeners: TList<IEnvironmentNotified<Notifier>>;
  public
    constructor Create(const ANotifier: Notifier);
    destructor Destroy(); override;

    procedure AddListener(const AListener: IEnvironmentNotified<Notifier>);
    procedure RemoveListener(const AListener: IEnvironmentNotified<Notifier>);

    procedure NotifyAll(const ANotification: TEnvironmentNotification;
      const AArgs: TObject);
  end;

  ENotificationCenterNotAvailable = class(Exception);

const
  /// <summary>
  ///   Tell the environment clients we are running async and they might schedule their tasks.
  /// </summary>
  INIT_NOTIFICATION = $0;
  BEFORE_SETUP_NOTIFICATION = $1;
  AFTER_SETUP_NOTIFICATION = $2;
  BEFORE_ACTIVATE_NOTIFICATION = $3;
  AFTER_ACTIVATE_NOTIFICATION = $4;
  BEFORE_DEACTIVATE_NOTIFICATION = $5;
  AFTER_DEACTIVATE_NOTIFICATION = $6;
  BEFORE_CREATE_ENVIRONMENT_NOTIFICATION = $7;
  AFTER_CREATE_ENVIRONMENT_NOTIFICATION = $8;
  BEFORE_UNZIP_NOTIFICATION = $9;
  AFTER_UNZIP_NOTIFICATION = $A;
  INTERNAL_READY_NOTIFICATION = $B;
  FINALIZE_NOTIFICATION = $FF;

type
  TPyEnvironmentNotifications = INIT_NOTIFICATION..FINALIZE_NOTIFICATION;

implementation

{ TEnvironmentBroadcaster }

constructor TEnvironmentBroadcaster<Notifier>.Create(const ANotifier: Notifier);
begin
  inherited Create();
  FNotifier := ANotifier;
  FListeners := TList<IEnvironmentNotified<Notifier>>.Create();
end;

destructor TEnvironmentBroadcaster<Notifier>.Destroy;
begin
  FListeners.Free();
end;

procedure TEnvironmentBroadcaster<Notifier>.AddListener(
  const AListener: IEnvironmentNotified<Notifier>);
begin
  FListeners.Add(AListener);
end;

procedure TEnvironmentBroadcaster<Notifier>.RemoveListener(
  const AListener: IEnvironmentNotified<Notifier>);
begin
  FListeners.Remove(AListener);
end;

procedure TEnvironmentBroadcaster<Notifier>.NotifyAll(
  const ANotification: TEnvironmentNotification; const AArgs: TObject);
var
  LListener: IEnvironmentNotified<Notifier>;
begin
  for LListener in FListeners do begin
    LListener.NotifyUpdate(FNotifier, ANotification, AArgs);
  end;
end;

end.
