unit PyTools.Notification.Protocol.Local;

interface

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.Generics.Collections,
  PyTools.Notification.Identifier,
  PyTools.Notification.Channel,
  PyTools.Notification.Protocol;

type
  TLocalNotificationProtocol = class(TInterfacedObject, IClientNotificationProtocol, IServerNotificationProtocol)
  private type
    TLocalQueue = class(TQueue<TNotificationChannel>);

    TLocalClient = class(TBaseClientNotificationProtocol)
    private
      FParent: TLocalNotificationProtocol;
      FTask: TThread;
    protected
      function TransmitContent(const AContent: TNotificationChannel): IAsyncResult; override;
      function ReceiveContent(): IAsyncResult; override;
    public
      constructor Create(const AParent: TLocalNotificationProtocol); reintroduce;
      destructor Destroy(); override;
    end;

    TLocalServer = class(TBaseServerNotificationProtocol)
    private
      FParent: TLocalNotificationProtocol;
      FTask: TThread;
    protected
      function TransmitContent(const AContent: TNotificationChannel): IAsyncResult; override;
      function ReceiveContent(): IAsyncResult; override;
    public
      constructor Create(const AParent: TLocalNotificationProtocol); reintroduce;
      destructor Destroy(); override;
    end;

    TNotificationAsyncResult = class(TBaseAsyncResult)
    private
      FAsyncTask: TProc;
    protected
      procedure Schedule; override;
      procedure AsyncDispatch; override;
    public
      constructor Create(const AContext: TObject; const AAsyncTask: TProc);
    end;

    TNotificationAsyncResult<TResult> = class(TNotificationAsyncResult)
    private
      FRetVal: TResult;
    public
      function GetRetVal: TResult;
    end;
  private
    // Protocols
    FClient: IClientNotificationProtocol;
    FServer: IServerNotificationProtocol;
    // Shared queues
    FClientQueue: TLocalQueue;
    FServerQueue: TLocalQueue;
    procedure ClearQueue(const AQueue: TLocalQueue);
  private
    property Client: IClientNotificationProtocol read FClient
      implements IClientNotificationProtocol;
    property Server: IServerNotificationProtocol read FServer
      implements IServerNotificationProtocol;
  public
    constructor Create();
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

  TAppNotificationProtocol = class
  private
    class var FInstance: TLocalNotificationProtocol;
    class function GetInstance: TLocalNotificationProtocol; static;
  public
    class destructor Destroy();

    class property Instance: TLocalNotificationProtocol read GetInstance;
  end;

implementation

{ TLocalNotificationProtocol.TLocalClient }

constructor TLocalNotificationProtocol.TLocalClient.Create(
  const AParent: TLocalNotificationProtocol);
begin
  inherited Create();
  FParent := AParent;
  FTask := TThread.CreateAnonymousThread(
    procedure()
    var
      LAsync: IAsyncResult;
      LNotification: TNotificationChannel;
    begin
      while not FTask.CheckTerminated() do begin
        if not Enabled then begin
          Sleep(100);
          Continue;
        end;

        try
          LAsync := ReceiveContent();
          LNotification := TNotificationAsyncResult<TNotificationChannel>(LAsync).GetRetVal();
          if Assigned(LNotification) then begin
            try
              Process(LNotification);
            finally
              LNotification.Free();
            end;
          end;
        except
          //
        end;
        Sleep(100);
      end;
      TThread.RemoveQueuedEvents(TThread.Current);
    end);
  FTask.FreeOnTerminate := false;
  FTask.Start();
end;

destructor TLocalNotificationProtocol.TLocalClient.Destroy;
begin
  FTask.Terminate();
  FTask.WaitFor();
  FTask.Free();
  inherited;
end;

function TLocalNotificationProtocol.TLocalClient.ReceiveContent: IAsyncResult;
var
  LAsyncResult: TNotificationAsyncResult<TNotificationChannel>;
begin
  LAsyncResult := TNotificationAsyncResult<TNotificationChannel>.Create(Self,
    procedure()
    begin
      if (FParent.FClientQueue.Count > 0) then
        LAsyncResult.FRetVal := FParent.FClientQueue.Dequeue()
      else
        LAsyncResult.FRetVal := nil;
    end);

  Result := LAsyncResult.Invoke();
end;

function TLocalNotificationProtocol.TLocalClient.TransmitContent(
  const AContent: TNotificationChannel): IAsyncResult;
begin
  Result := TNotificationAsyncResult.Create(Self, procedure() begin
    FParent.FServerQueue.Enqueue(AContent);
  end).Invoke();
end;

{ TLocalNotificationProtocol.TLocalServer }

constructor TLocalNotificationProtocol.TLocalServer.Create(
  const AParent: TLocalNotificationProtocol);
begin
  inherited Create();
  FParent := AParent;
  FTask := TThread.CreateAnonymousThread(
    procedure()
    var
      LAsync: IAsyncResult;
      LNotification: TNotificationChannel;
    begin
      while not FTask.CheckTerminated() do begin
        if not Enabled then begin
          Sleep(100);
          Continue;
        end;

        LAsync := ReceiveContent();
        LNotification := TNotificationAsyncResult<TNotificationChannel>(LAsync).GetRetVal();
        if Assigned(LNotification) then begin
          try
            Process(LNotification);
          finally
            LNotification.Free();
          end;
        end;
        Sleep(100);
      end;
      TThread.RemoveQueuedEvents(TThread.Current);
    end);
  FTask.FreeOnTerminate := false;
  FTask.Start();
end;

destructor TLocalNotificationProtocol.TLocalServer.Destroy;
begin
  FTask.Terminate();
  FTask.WaitFor();
  FTask.Free();
  inherited;
end;

function TLocalNotificationProtocol.TLocalServer.ReceiveContent: IAsyncResult;
var
  LAsyncResult: TNotificationAsyncResult<TNotificationChannel>;
begin
  LAsyncResult := TNotificationAsyncResult<TNotificationChannel>.Create(Self,
    procedure()
    begin
      if (FParent.FServerQueue.Count > 0) then
        LAsyncResult.FRetVal := FParent.FServerQueue.Dequeue
      else
        LAsyncResult.FRetVal := nil;
    end);

  Result := LAsyncResult.Invoke();
end;

function TLocalNotificationProtocol.TLocalServer.TransmitContent(
  const AContent: TNotificationChannel): IAsyncResult;
begin
  Result := TNotificationAsyncResult.Create(Self, procedure() begin
    FParent.FClientQueue.Enqueue(AContent);
  end).Invoke();
end;

{ TLocalNotificationProtocol.TNotificationAsyncResult }

constructor TLocalNotificationProtocol.TNotificationAsyncResult.Create(
  const AContext: TObject; const AAsyncTask: TProc);
begin
  inherited Create(AContext);
  FAsyncTask := AAsyncTask;
end;

procedure TLocalNotificationProtocol.TNotificationAsyncResult.Schedule;
begin
  TThread.ForceQueue(TThread.Current, DoAsyncDispatch);
end;

procedure TLocalNotificationProtocol.TNotificationAsyncResult.AsyncDispatch;
begin
  FAsyncTask();
end;

{ TLocalNotificationProtocol.TNotificationAsyncResult<TResult> }

function TLocalNotificationProtocol.TNotificationAsyncResult<TResult>.GetRetVal: TResult;
begin
  WaitForCompletion;
  Result := FRetVal;
end;

{ TLocalNotificationProtocol }

procedure TLocalNotificationProtocol.ClearQueue(const AQueue: TLocalQueue);
begin
  while (AQueue.Count > 0) do
    AQueue.Dequeue().Free();
end;

constructor TLocalNotificationProtocol.Create;
begin
  inherited;
  FClientQueue := TLocalQueue.Create();
  FServerQueue := TLocalQueue.Create();
  FClient := TLocalClient.Create(Self);
  FServer := TLocalServer.Create(Self);
end;

destructor TLocalNotificationProtocol.Destroy;
begin
  FServer := nil;
  FClient := nil;
  ClearQueue(FServerQueue);
  ClearQueue(FClientQueue);
  FServerQueue.Free();
  FClientQueue.Free();
  inherited;
end;

function TLocalNotificationProtocol.BroadcastEvent(
  const AEvent: TEventChannel): IAwait;
begin
  Result := (FServer as TBaseServerNotificationProtocol).BroadcastEvent(AEvent);
end;

function TLocalNotificationProtocol.SubscribeToEvent(
  const AEventChannelIdentifier: TEventChannelIdentifier;
  const AEventNotification: TEventNotificationCallback): IDisconnectable;
begin
  Result := (FClient as TBaseClientNotificationProtocol).SubscribeToEvent(
    AEventChannelIdentifier, AEventNotification);
end;

function TLocalNotificationProtocol.SubscribeToEvent<TEvent>(
  const AEventNotification: TEventNotificationCallback<TEvent>): IDisconnectable;
begin
  Result := (FClient as TBaseClientNotificationProtocol).SubscribeToEvent<TEvent>(
    AEventNotification);
end;

function TLocalNotificationProtocol.SendRequest(const ARequest: TRequestChannel;
  const AAccept: TRequestNotificationAcceptCallback<TResponseChannel>;
  const AReject: TRequestNotificationRejectCallback): IAwait;
begin
  Result := (FClient as TBaseClientNotificationProtocol).SendRequest(ARequest,
    AAccept, AReject);
end;

function TLocalNotificationProtocol.SendRequest<TResponse>(
  const ARequest: TRequestChannel;
  const AAccept: TRequestNotificationAcceptCallback<TResponse>;
  const AReject: TRequestNotificationRejectCallback): IAwait;
begin
  Result := (FClient as TBaseClientNotificationProtocol).SendRequest<TResponse>(
    ARequest, AAccept, AReject);
end;

function TLocalNotificationProtocol.SubscribeToRequest(
  const ARequestChannelIdentifier: TRequestChannelIdentifier;
  const ARequestNotification: TRequestNotificationCallback): IDisconnectable;
begin
  Result := (FServer as TBaseServerNotificationProtocol).SubscribeToRequest(
    ARequestChannelIdentifier, ARequestNotification);
end;

function TLocalNotificationProtocol.SubscribeToRequest<TRequest>(
  const ARequestNotification: TRequestNotificationCallback<TRequest>): IDisconnectable;
begin
  Result := (FServer as TBaseServerNotificationProtocol).SubscribeToRequest<TRequest>(
    ARequestNotification);
end;

{ TAppNotificationProtocol }

class destructor TAppNotificationProtocol.Destroy;
begin
  FInstance.Free();
end;

class function TAppNotificationProtocol.GetInstance: TLocalNotificationProtocol;
begin
  if not Assigned(FInstance) then
    FInstance := TLocalNotificationProtocol.Create();
  Result := FInstance;
end;

end.
