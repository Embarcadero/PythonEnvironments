(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyTools.Protocol.Local'                                 *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality: Notification channels, channel identifiers             *)
(*                 and contents                                           *)
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
unit PyTools.Notification.Channel;

interface

uses
  System.SysUtils,
  System.SyncObjs;

type
  TRequestChannel = class;
  TResponseChannel = class;
  TErrorResponseChannel = class;
  TEventChannel = class;

  TRequestNotificationAcceptCallback<T: TResponseChannel> = reference to procedure(const Arg: T);
  TRequestNotificationRejectCallback = reference to procedure(const Arg: TErrorResponseChannel);

  TEventNotificationCallback<T: TEventChannel> = reference to procedure(const Arg: T);
  TEventNotificationCallback = TEventNotificationCallback<TEventChannel>;

  TRequestNotificationCallback<TRequest: TRequestChannel> = reference to function(const Arg: TRequest): TResponseChannel;
  TRequestNotificationCallback = TRequestNotificationCallback<TRequestChannel>;

  {$SCOPEDENUMS ON}
  TNotificationChannelIdentifier = (Event, Request, Response);
  {$SCOPEDENUMS OFF}

  TRequestChannelIdentifier = string;
  TEventChannelIdentifier = string;

  TEmptyBody = class
  end;

  TEmptyArgument = class
  end;

  TErrorResponseBody = class
  private
    FCode: integer;
    FMessage: string;
  public
    property Code: integer read FCode write FCode;
    property Message: string read FMessage write FMessage;
  end;

  ChannelTypeAttribute = class(TCustomAttribute)
  private
    FChannelType: TNotificationChannelIdentifier;
  public
    constructor Create(const AChannelType: TNotificationChannelIdentifier);

    property ChannelType: TNotificationChannelIdentifier read FChannelType;
  end;

  RequestChannelAttribute = class(TCustomAttribute)
  private
    FIdentifier: TRequestChannelIdentifier;
  public
    constructor Create(const AIdentifier: TRequestChannelIdentifier);

    property Identifier: TRequestChannelIdentifier read FIdentifier;
  end;

  EventTypeAttribute = class(TCustomAttribute)
  private
    FEventType: TEventChannelIdentifier;
  public
    constructor Create(const AEventType: TEventChannelIdentifier);

    property EventType: TEventChannelIdentifier read FEventType;
  end;

  TNotificationId = integer;

  TNotificationChannel = class
  private
    class var FNextId: TNotificationId;
  private
    FChannelType: TNotificationChannelIdentifier;
    FId: TNotificationId;
  protected
    procedure GenerateId();
  protected
    class function InitializeType<T>(): T;
    class procedure FinalizeType<T>(var AValue);
  public
    constructor Create();
    class constructor Create();

    class function GetNotificationChannelType(): TNotificationChannelIdentifier;

    property ChannelType: TNotificationChannelIdentifier read FChannelType;
    property Id: TNotificationId read FId;
  end;

  [ChannelType(TNotificationChannelIdentifier.Request)]
  TRequestChannel = class(TNotificationChannel)
  private
    FRequestType: TRequestChannelIdentifier;
  public
    class function GetIdentifier(): TRequestChannelIdentifier;
  public
    constructor Create();

    property RequestType: TRequestChannelIdentifier read FRequestType;
  end;

  TRequestChannel<TArguments> = class(TRequestChannel)
  private
    FArguments: TArguments;
  public
    constructor Create();
    destructor Destroy(); override;

    property Arguments: TArguments read FArguments write FArguments;
  end;

  [ChannelType(TNotificationChannelIdentifier.Response)]
  TResponseChannel = class(TNotificationChannel)
  private
    function GetId: TNotificationId;
    procedure SetId(const Value: TNotificationId);
  public
    constructor Create();
    destructor Destroy(); override;

    property Id: TNotificationId read GetId write SetId;
  end;

  TResponseChannel<TBody> = class(TResponseChannel)
  private
    FBody: TBody;
  public
    constructor Create();
    destructor Destroy(); override;

    property Body: TBody read FBody;
  end;

  TErrorResponseChannel = class(TResponseChannel);

  TErrorResponseChannel<TBody> = class(TErrorResponseChannel)
  private
    FBody: TBody;
  public
    constructor Create(); overload;
    destructor Destroy(); override;

    property Body: TBody read FBody;
  end;

  [ChannelType(TNotificationChannelIdentifier.Event)]
  TEventChannel = class(TNotificationChannel)
  private
    FEventType: TEventChannelIdentifier;
  public
    class function GetEventChannelType(): TEventChannelIdentifier;
  public
    constructor Create();

    property EventType: TEventChannelIdentifier read FEventType;
  end;

  TEventChannel<TBody> = class(TEventChannel)
  private
    FBody: TBody;
  public
    constructor Create();
    destructor Destroy(); override;

    property Body: TBody read FBody;
  end;

implementation

uses
  System.Rtti,
  PyTools.Exception;

{ RequestChannelAttribute }

constructor RequestChannelAttribute.Create(const AIdentifier: TRequestChannelIdentifier);
begin
  inherited Create();
  FIdentifier := AIdentifier;
end;

{ EventTypeAttribute }

constructor EventTypeAttribute.Create(const AEventType: TEventChannelIdentifier);
begin
  inherited Create();
  FEventType := AEventType;
end;

{ ChannelTypeAttribute }

constructor ChannelTypeAttribute.Create(
  const AChannelType: TNotificationChannelIdentifier);
begin
  inherited Create();
  FChannelType := AChannelType;
end;

{ TNotificationChannel }

class constructor TNotificationChannel.Create;
begin
  FNextId := 0;
end;

constructor TNotificationChannel.Create;
begin
  inherited;
  FChannelType := GetNotificationChannelType();
end;

procedure TNotificationChannel.GenerateId;
begin
  TInterlocked.CompareExchange(FNextId, 0, High(Integer));
  FId := TInterlocked.Add(FNextId, 1);
end;

class function TNotificationChannel.InitializeType<T>: T;
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(TypeInfo(T));
  if LRttiType.IsInstance then
    for var LRttiMethod in LRttiType.GetMethods() do
      if LRttiMethod.HasExtendedInfo and LRttiMethod.IsConstructor and (Length(LRttiMethod.GetParameters()) = 0) then
        Exit(LRttiMethod.Invoke(LRttiType.AsInstance.MetaclassType, []).AsType<T>());

  Result := Default(T);
end;

class procedure TNotificationChannel.FinalizeType<T>(var AValue);
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(TypeInfo(T));
  if LRttiType.IsInstance then
    TObject(AValue).Free();
end;

class function TNotificationChannel.GetNotificationChannelType: TNotificationChannelIdentifier;
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(Self.ClassInfo);
  while Assigned(LRttiType) do begin
    var LAttribute := LRttiType.GetAttribute<ChannelTypeAttribute>();
    if Assigned(LAttribute) then
      Exit(LAttribute.ChannelType);

    LRttiType := LRttiType.BaseType;
  end;

  raise EChannelTypeAttributeNotFound.Create('Channel type attribute not found.');
end;

{ TRequestChannel }

constructor TRequestChannel.Create;
begin
  inherited;
  FRequestType := GetIdentifier();
  GenerateId();
end;

class function TRequestChannel.GetIdentifier: TRequestChannelIdentifier;
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(Self.ClassInfo);
  while Assigned(LRttiType) do begin
    var LAttribute := LRttiType.GetAttribute<RequestChannelAttribute>();
    if Assigned(LAttribute) then
      Exit(LAttribute.Identifier);

    LRttiType := LRttiType.BaseType;
  end;

  Assert(false, 'Request type attribute not found.');
end;

{ TRequestChannel<TArguments> }

constructor TRequestChannel<TArguments>.Create;
begin
  inherited;
  FArguments := TNotificationChannel.InitializeType<TArguments>();
end;

destructor TRequestChannel<TArguments>.Destroy;
begin
  TNotificationChannel.FinalizeType<TArguments>(FArguments);
  inherited;
end;

{ TResponseChannel }

constructor TResponseChannel.Create;
begin
  inherited;
end;

destructor TResponseChannel.Destroy;
begin
  inherited;
end;

function TResponseChannel.GetId: TNotificationId;
begin
  Result := FId;
end;

procedure TResponseChannel.SetId(const Value: TNotificationId);
begin
  FId := Value;
end;

{ TResponseChannel<TBody> }

constructor TResponseChannel<TBody>.Create;
begin
  inherited;
  FBody := TNotificationChannel.InitializeType<TBody>();
end;

destructor TResponseChannel<TBody>.Destroy;
begin
  TNotificationChannel.FinalizeType<TBody>(FBody);
  inherited;
end;

{ TEventChannel }

constructor TEventChannel.Create;
begin
  inherited;
  FEventType := GeTEventChannelType();
  GenerateId();
end;

class function TEventChannel.GeTEventChannelType: TEventChannelIdentifier;
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(Self.ClassInfo);
  while Assigned(LRttiType) do begin
    var LAttribute := LRttiType.GetAttribute<EventTypeAttribute>();
    if Assigned(LAttribute) then
      Exit(LAttribute.EventType);

    LRttiType := LRttiType.BaseType;
  end;

  Assert(false, 'Event type attribute not found.');
end;

{ TEventChannel<TBody> }

constructor TEventChannel<TBody>.Create;
begin
  inherited;
  FBody := TNotificationChannel.InitializeType<TBody>();
end;

destructor TEventChannel<TBody>.Destroy;
begin
  TNotificationChannel.FinalizeType<TBody>(FBody);
  inherited;
end;

{ TErrorResponseChannel<TBody> }

constructor TErrorResponseChannel<TBody>.Create();
begin
  inherited Create();
  FBody := TNotificationChannel.InitializeType<TBody>();
end;

destructor TErrorResponseChannel<TBody>.Destroy;
begin
  TNotificationChannel.FinalizeType<TBody>(FBody);
  inherited;
end;

end.
