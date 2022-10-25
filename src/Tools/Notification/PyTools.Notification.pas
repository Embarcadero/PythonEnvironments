unit PyTools.Notification;

interface

uses
  PyTools.Notification.Identifier,
  PyTools.Notification.Content,
  PyTools.Notification.Channel,
  PyTools.Notification.Protocol,
  PyTools.Notification.Protocol.Local;

type
  TNotificationChannelIdentifier = PyTools.Notification.Identifier.TNotificationChannelIdentifier;
  TRequestChannelIdentifier = PyTools.Notification.Identifier.TRequestChannelIdentifier;
  TEventChannelIdentifier = PyTools.Notification.Identifier.TEventChannelIdentifier;

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

  TEmptyBody = PyTools.Notification.Content.TEmptyBody;
  TEmptyArgument = PyTools.Notification.Content.TEmptyArgument;
  TErrorResponseBody = PyTools.Notification.Content.TErrorResponseBody;

  IDisconnectable = PyTools.Notification.Protocol.IDisconnectable;
  IAwait = PyTools.Notification.Protocol.IAwait;
  IClientNotificationProtocol = PyTools.Notification.Protocol.IClientNotificationProtocol;
  IServerNotificationProtocol = PyTools.Notification.Protocol.IServerNotificationProtocol;

  TLocalNotificationProtocol = PyTools.Notification.Protocol.Local.TLocalNotificationProtocol;
  TAppNotificationProtocol = PyTools.Notification.Protocol.Local.TAppNotificationProtocol;

implementation

end.
