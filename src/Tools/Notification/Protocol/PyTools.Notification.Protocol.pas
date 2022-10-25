unit PyTools.Notification.Protocol;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Threading,
  System.Generics.Collections,
  PyTools.Notification.Identifier,
  PyTools.Notification.Content,
  PyTools.Notification.Channel;

type
  IDisconnectable = interface
    ['{21B1CCF8-D844-4738-BB3E-31C826B33949}']
    procedure Disconnect();
  end;

  IAwait = interface
    ['{AF7099CE-BBBE-43EF-AD18-78FEA86BFCC2}']
    function Wait(const ATimeOut: cardinal): boolean;
  end;

  IClientNotificationProtocol = interface
    ['{AAF24EDA-B2E3-47F1-93E9-6C3B112A9EBB}']
    /// <summary>
    ///   Event subscription.
    /// </summary>
    function SubscribeToEvent(const AEventChannelIdentifier: TEventChannelIdentifier;
      const AEventNotification: TEventNotificationCallback): IDisconnectable;
    /// <summary>
    ///   Request handler.
    ///   Use IAwait to guarantee message delivery.
    /// </summary>
    function SendRequest(const ARequest: TRequestChannel;
      const AAccept: TRequestNotificationAcceptCallback<TResponseChannel>;
      const AReject: TRequestNotificationRejectCallback): IAwait;
  end;

  IServerNotificationProtocol = interface
    ['{936441AE-95B1-4952-BA67-DAD15EAFBE7B}']
    /// <summary>
    ///   Event subscription.
    /// </summary>
    function SubscribeToRequest(const ARequestChannelIdentifier: TRequestChannelIdentifier;
      const ARequestNotification: TRequestNotificationCallback): IDisconnectable;
    /// <summary>
    ///   Event broadcaster.
    /// </summary>
    function BroadcastEvent(const AEvent: TEventChannel): IAwait;
    /// <summary>
    ///   Response handler.
    ///   Use IAwait to guarantee message delivery.
    /// </summary>
    function SendResponse(const AResponse: TResponseChannel): IAwait;
  end;

  TBaseNotificationProtocol = class(TInterfacedObject)
  strict private
    FEnabled: boolean;
  protected type
    TAwait = class(TInterfacedObject, IAwait)
    private
      FAsyncResult: IAsyncResult;
    public
      constructor Create(const AAsyncResult: IAsyncResult);
      function Wait(const ATimeOut: cardinal): boolean;
    end;
    TDisconnectable = class(TInterfacedObject, IDisconnectable)
    private type
      TOnDisconnect = reference to procedure();
    private
      FOnDisconnect: TOnDisconnect;
    public
      constructor Create(const AOnDisconnect: TOnDisconnect);

      procedure Disconnect();
    end;
  protected
    function TransmitContent(const AContent: TNotificationChannel): IAsyncResult; virtual; abstract;
    function ReceiveContent(): IAsyncResult; virtual; abstract;

    procedure Process(const AContent: TNotificationChannel);
    procedure ProcessEvent(const AEvent: TEventChannel); virtual; abstract;
    procedure ProcessRequest(const ARequest: TRequestChannel); virtual; abstract;
    procedure ProcessResponse(const AResponse: TResponseChannel); virtual; abstract;

    // Helpers
    procedure SafeAccess(const AInstance: TObject; const AProc: TProc);
  public
    constructor Create(); virtual;

    property Enabled: boolean read FEnabled write FEnabled;
  end;

  TBaseClientNotificationProtocol = class(TBaseNotificationProtocol, IClientNotificationProtocol)
  private type
    TRequestCallbackMethods = TPair<
      TRequestNotificationAcceptCallback<TResponseChannel>,
      TRequestNotificationRejectCallback>;
    TIdentifiedEventCallbacks = TThreadList<TEventNotificationCallback>;
  private
    FEventCallbacks: TObjectDictionary<TEventChannelIdentifier, TIdentifiedEventCallbacks>;
    FRequestCallbacks: TDictionary<TNotificationId, TRequestCallbackMethods>;
  protected
    procedure ProcessEvent(const AEvent: TEventChannel); override;
    procedure ProcessRequest(const ARequest: TRequestChannel); override;
    procedure ProcessResponse(const AResponse: TResponseChannel); override;
  public
    constructor Create(); override;
    destructor Destroy(); override;
    /// <summary>
    ///   Event subscription.
    /// </summary>
    function SubscribeToEvent(const AEventChannelIdentifier: TEventChannelIdentifier;
      const AEventNotification: TEventNotificationCallback): IDisconnectable; overload;
    /// <summary>
    ///   Generic event subscription.
    /// </summary>
    function SubscribeToEvent<TEvent: TEventChannel>(
      const AEventNotification: TEventNotificationCallback<TEvent>): IDisconnectable; overload;
    /// <summary>
    ///   Request handler.
    ///   Use IAwait to guarantee message delivery.
    /// </summary>
    function SendRequest(const ARequest: TRequestChannel;
      const AAccept: TRequestNotificationAcceptCallback<TResponseChannel>;
      const AReject: TRequestNotificationRejectCallback): IAwait; overload;
    /// <summary>
    ///   Generic request handler.
    ///   Use IAwait to guarantee message delivery.
    /// </summary>
    function SendRequest<TResponse: TResponseChannel>(const ARequest: TRequestChannel;
      const AAccept: TRequestNotificationAcceptCallback<TResponse>;
      const AReject: TRequestNotificationRejectCallback): IAwait; overload;
  end;

  TBaseServerNotificationProtocol = class(TBaseNotificationProtocol, IServerNotificationProtocol)
  private
    FRequestCallbacks: TDictionary<TRequestChannelIdentifier, TRequestNotificationCallback>;
  protected
    procedure ProcessEvent(const AEvent: TEventChannel); override;
    procedure ProcessRequest(const ARequest: TRequestChannel); override;
    procedure ProcessResponse(const AResponse: TResponseChannel); override;
    function SendResponse(const AResponse: TResponseChannel): IAwait;
  public
    constructor Create(); override;
    destructor Destroy(); override;
    /// <summary>
    ///   Event subscription.
    /// </summary>
    function SubscribeToRequest(const ARequestChannelIdentifier: TRequestChannelIdentifier;
      const ARequestNotification: TRequestNotificationCallback): IDisconnectable; overload;
    /// <summary>
    ///   Generic event subscription.
    /// </summary>
    function SubscribeToRequest<TRequest: TRequestChannel>(
      const ARequestNotification: TRequestNotificationCallback<TRequest>): IDisconnectable; overload;
    /// <summary>
    ///   Event broadcaster.
    /// </summary>
    function BroadcastEvent(const AEvent: TEventChannel): IAwait;
  end;

implementation

{ TBaseNotificationProtocol.TAwait }

constructor TBaseNotificationProtocol.TAwait.Create(
  const AAsyncResult: IAsyncResult);
begin
  inherited Create();
  FAsyncResult := AAsyncResult;
end;

function TBaseNotificationProtocol.TAwait.Wait(const ATimeOut: cardinal): boolean;
begin
  Result := FAsyncResult.AsyncWaitEvent.WaitFor(ATimeOut) = TWaitResult.wrSignaled;
  if Result then
    TBaseAsyncResult(FAsyncResult).WaitForCompletion();
end;

{ TBaseNotificationProtocol.TDisconnectable }

constructor TBaseNotificationProtocol.TDisconnectable.Create(
  const AOnDisconnect: TOnDisconnect);
begin
  inherited Create();
  FOnDisconnect := AOnDisconnect;
end;

procedure TBaseNotificationProtocol.TDisconnectable.Disconnect;
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect();
  FOnDisconnect := nil;
end;

{ TBaseNotificationProtocol }

constructor TBaseNotificationProtocol.Create;
begin
  inherited Create();
  FEnabled := true;
end;

procedure TBaseNotificationProtocol.Process(
  const AContent: TNotificationChannel);
begin
  case AContent.ChannelType of
    TNotificationChannelIdentifier.Event:
      ProcessEvent(AContent as TEventChannel);
    TNotificationChannelIdentifier.Request:
      ProcessRequest(AContent as TRequestChannel);
    TNotificationChannelIdentifier.Response:
      ProcessResponse(AContent as TResponseChannel);
  end;
end;

procedure TBaseNotificationProtocol.SafeAccess(const AInstance: TObject;
  const AProc: TProc);
begin
  Assert(Assigned(AProc), 'Invalid argument "AProc".');

  if not Assigned(AInstance) then
    Exit;

  TMonitor.Enter(AInstance);
  try
    AProc();
    TMonitor.PulseAll(AInstance);
  finally
    TMonitor.Exit(AInstance);
  end;
end;

{ TBaseClientNotificationProtocol }

constructor TBaseClientNotificationProtocol.Create;
begin
  inherited;
  FEventCallbacks := TObjectDictionary<
    TEventChannelIdentifier,
    TIdentifiedEventCallbacks>.Create([doOwnsValues]);
  FRequestCallbacks := TDictionary<TNotificationId, TRequestCallbackMethods>.Create();
end;

destructor TBaseClientNotificationProtocol.Destroy;
begin
  FRequestCallbacks.Free();
  FEventCallbacks.Free();
  inherited;
end;

procedure TBaseClientNotificationProtocol.ProcessEvent(
  const AEvent: TEventChannel);
var
  LCallbacks: TIdentifiedEventCallbacks;
  LIdentifierCallbacks: TList<TEventNotificationCallback>;
  I: Integer;
begin
  SafeAccess(FEventCallbacks, procedure()
  begin
    LCallbacks := FEventCallbacks[AEvent.EventType];
  end);

  if Assigned(LCallbacks) then begin
    LIdentifierCallbacks := LCallbacks.LockList();
    try
      // Broadcasting events inside this critical section avoids
      // notification for unsubscribing peers
      for I := 0 to LIdentifierCallbacks.Count - 1 do begin
        try
          LIdentifierCallbacks[I](AEvent);
        except
          //
        end;
      end;
    finally
      LCallbacks.UnlockList();
    end;
  end;
end;

procedure TBaseClientNotificationProtocol.ProcessRequest(
  const ARequest: TRequestChannel);
begin
  Assert(false, 'Unable to process request in the client side.');
end;

procedure TBaseClientNotificationProtocol.ProcessResponse(
  const AResponse: TResponseChannel);
var
  LCallbacks: TRequestCallbackMethods;
begin
  SafeAccess(FRequestCallbacks, procedure() begin
    if FRequestCallbacks.ContainsKey(AResponse.Id) then begin
      try
        LCallbacks := FRequestCallbacks[AResponse.Id];

        if AResponse.InheritsFrom(TErrorResponseChannel) then
          LCallbacks.Value(TErrorResponseChannel(AResponse))
        else
          LCallbacks.Key(AResponse);
      finally
        FRequestCallbacks.Remove(AResponse.Id);
      end;
    end;
  end);
end;

function TBaseClientNotificationProtocol.SubscribeToEvent(
  const AEventChannelIdentifier: TEventChannelIdentifier;
  const AEventNotification: TEventNotificationCallback): IDisconnectable;
var
  LCallbacks: TIdentifiedEventCallbacks;
  LIdentifierCallbacks: TList<TEventNotificationCallback>;
begin
  Assert(not AEventChannelIdentifier.Trim().IsEmpty(), 'Invalid argument "AEventChannelIdentifier".');

  if not Enabled then
    Exit(nil);

  SafeAccess(FEventCallbacks, procedure() begin
    if not FEventCallbacks.ContainsKey(AEventChannelIdentifier) then
      FEventCallbacks.Add(
        AEventChannelIdentifier,
        TIdentifiedEventCallbacks.Create());

    LCallbacks := FEventCallbacks[AEventChannelIdentifier];
  end);

  if Assigned(LCallbacks) then begin
    LIdentifierCallbacks := LCallbacks.LockList();
    try
       LIdentifierCallbacks.Add(AEventNotification);
    finally
      LCallbacks.UnlockList();
    end;
  end;

  Result := TDisconnectable.Create(procedure() begin
    SafeAccess(FEventCallbacks, procedure() begin
      LCallbacks := FEventCallbacks[AEventChannelIdentifier];
    end);

    if Assigned(LCallbacks) then
      LIdentifierCallbacks := LCallbacks.LockList();
      try
         LIdentifierCallbacks.Remove(AEventNotification);
      finally
        LCallbacks.UnlockList();
      end;
  end);
end;

function TBaseClientNotificationProtocol.SubscribeToEvent<TEvent>(
  const AEventNotification: TEventNotificationCallback<TEvent>): IDisconnectable;
begin
  Result := SubscribeToEvent(TEvent.GetEventChannelType(),
    procedure(const Arg: TEventChannel)
    begin
      if (Arg is TEvent) then
        AEventNotification(Arg);
    end);
end;

function TBaseClientNotificationProtocol.SendRequest(
  const ARequest: TRequestChannel;
  const AAccept: TRequestNotificationAcceptCallback<TResponseChannel>;
  const AReject: TRequestNotificationRejectCallback): IAwait;
begin
  Assert(Assigned(ARequest), 'Invalid argument "ARequest".');

  if not Enabled then
    Exit(nil);

  SafeAccess(FEventCallbacks, procedure() begin
    FRequestCallbacks.Add(
      ARequest.Id,
      TRequestCallbackMethods.Create(AAccept, AReject));
  end);

  Result := TAwait.Create(TransmitContent(ARequest));
end;

function TBaseClientNotificationProtocol.SendRequest<TResponse>(
  const ARequest: TRequestChannel;
  const AAccept: TRequestNotificationAcceptCallback<TResponse>;
  const AReject: TRequestNotificationRejectCallback): IAwait;
begin
  Result := SendRequest(ARequest,
    procedure(const Arg: TResponseChannel)
    begin
      AAccept(Arg as TResponse);
    end, AReject);
end;

{ TBaseServerNotificationProtocol }

constructor TBaseServerNotificationProtocol.Create;
begin
  inherited Create();
  FRequestCallbacks := TDictionary<TRequestChannelIdentifier, TRequestNotificationCallback>.Create();
end;

destructor TBaseServerNotificationProtocol.Destroy;
begin
  FRequestCallbacks.Free();
  inherited;
end;

procedure TBaseServerNotificationProtocol.ProcessEvent(
  const AEvent: TEventChannel);
begin
  Assert(false, 'Unable to process event in the server side.');
end;

procedure TBaseServerNotificationProtocol.ProcessRequest(
  const ARequest: TRequestChannel);
var
  LCallback: TRequestNotificationCallback;
  LResponse: TResponseChannel;
begin
  TMonitor.Enter(FRequestCallbacks);
  try
    LCallback := FRequestCallbacks[ARequest.RequestType];
    TMonitor.PulseAll(FRequestCallbacks);
  finally
    TMonitor.Exit(FRequestCallbacks);
  end;

  if Assigned(LCallback) then begin
    LResponse := LCallback(ARequest);
    LResponse.Id := ARequest.Id;
    SendResponse(LResponse);
  end;
end;

procedure TBaseServerNotificationProtocol.ProcessResponse(
  const AResponse: TResponseChannel);
begin
  Assert(false, 'Unable to process response in the server side.');
end;

function TBaseServerNotificationProtocol.BroadcastEvent(
  const AEvent: TEventChannel): IAwait;
begin
  Assert(Assigned(AEvent), 'Invalid argument "AEvent".');

  Result := TAwait.Create(TransmitContent(AEvent));
end;

function TBaseServerNotificationProtocol.SendResponse(
  const AResponse: TResponseChannel): IAwait;
begin
  Result := TAwait.Create(TransmitContent(AResponse));
end;

function TBaseServerNotificationProtocol.SubscribeToRequest(
  const ARequestChannelIdentifier: TRequestChannelIdentifier;
  const ARequestNotification: TRequestNotificationCallback): IDisconnectable;
begin
  Assert(not ARequestChannelIdentifier.Trim().IsEmpty(), 'Invalid argument "ARequestChannelIdentifier".');

  if not Enabled then
    Exit(nil);

  SafeAccess(FRequestCallbacks, procedure() begin
    FRequestCallbacks.Add(ARequestChannelIdentifier, ARequestNotification);
  end);

  Result := TDisconnectable.Create(procedure() begin
    SafeAccess(FRequestCallbacks, procedure() begin
      FRequestCallbacks.Remove(ARequestChannelIdentifier);
    end);
  end);
end;

function TBaseServerNotificationProtocol.SubscribeToRequest<TRequest>(
  const ARequestNotification: TRequestNotificationCallback<TRequest>): IDisconnectable;
begin
  Result := SubscribeToRequest(TRequest.GetIdentifier(),
    function(const Arg: TRequestChannel): TResponseChannel begin
      if (Arg is TRequest) then
        Result := ARequestNotification(Arg)
      else
        Result := nil;
    end);
end;

end.
