(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyTools.Notification'                                   *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality: Notification types aliases                             *)
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
unit PyTools.Notification;

interface

uses
  PyTools.Notification.Channel,
  PyTools.Notification.Protocol,
  PyTools.Notification.Protocol.Local;

type
  TNotificationChannelIdentifier = PyTools.Notification.Channel.TNotificationChannelIdentifier;
  TRequestChannelIdentifier = PyTools.Notification.Channel.TRequestChannelIdentifier;
  TEventChannelIdentifier = PyTools.Notification.Channel.TEventChannelIdentifier;

  TEventNotificationCallback<T: TEventChannel> = reference to procedure(const Arg: T);
  TEventNotificationCallback = PyTools.Notification.Channel.TEventNotificationCallback<TEventChannel>;

  RequestTypeAttribute = PyTools.Notification.Channel.RequestChannelAttribute;
  EventTypeAttribute = PyTools.Notification.Channel.EventTypeAttribute;

  TRequestChannel = PyTools.Notification.Channel.TRequestChannel;
  TRequestChannel<TArguments> = class(PyTools.Notification.Channel.TRequestChannel<TArguments>);
  TResponseChannel = PyTools.Notification.Channel.TResponseChannel;
  TResponseChannel<TBody> = class(PyTools.Notification.Channel.TResponseChannel<TBody>);
  TErrorResponseChannel = PyTools.Notification.Channel.TErrorResponseChannel;
  TErrorResponseChannel<TBody: TErrorResponseBody> = class(PyTools.Notification.Channel.TErrorResponseChannel<TBody>);
  TEventChannel = PyTools.Notification.Channel.TEventChannel;
  TEventChannel<TBody> = class(PyTools.Notification.Channel.TEventChannel<TBody>);

  TEmptyBody = PyTools.Notification.Channel.TEmptyBody;
  TEmptyArgument = PyTools.Notification.Channel.TEmptyArgument;
  TErrorResponseBody = PyTools.Notification.Channel.TErrorResponseBody;

  IDisconnectable = PyTools.Notification.Protocol.IDisconnectable;
  IAwait = PyTools.Notification.Protocol.IAwait;
  IClientNotificationProtocol = PyTools.Notification.Protocol.IClientNotificationProtocol;
  IServerNotificationProtocol = PyTools.Notification.Protocol.IServerNotificationProtocol;

  TLocalNotificationProtocol = PyTools.Notification.Protocol.Local.TLocalNotificationProtocol;
  TAppNotificationProtocol = PyTools.Notification.Protocol.Local.TAppNotificationProtocol;

implementation

end.
