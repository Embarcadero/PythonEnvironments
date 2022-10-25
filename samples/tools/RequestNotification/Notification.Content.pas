unit Notification.Content;

interface

uses
  PyTools.Notification;

const
  REQUEST_CHANNEL_IDENTIFIER_BASIC = 'basic';

type
  TBasicRequestArgument = class
  public
    RequestTime: TTime;
  end;

  [RequestType(REQUEST_CHANNEL_IDENTIFIER_BASIC)]
  TBasicRequest = class(TRequestChannel<TBasicRequestArgument>)
  public
    constructor Create(const ARequestTime: TTime);
  end;

  TBasicResponseBody = class
  public
    ResponseTime: TTime;
  end;

  TBasicResponse = class(TResponseChannel<TBasicResponseBody>)
  public
    constructor Create(const AResponseTime: TTime);
  end;

   TBasicResponseError = class(TErrorResponseChannel<TErrorResponseBody>)
   public
    constructor Create(const AMessage: string);
   end;

implementation

{ TBasicRequest }

constructor TBasicRequest.Create(const ARequestTime: TTime);
begin
  inherited Create();
  Arguments.RequestTime := ARequestTime;
end;

{ TBasicResponse }

constructor TBasicResponse.Create(const AResponseTime: TTime);
begin
  inherited Create();
  Body.ResponseTime := AResponseTime;
end;

{ TBasicResponseError<TBody> }

constructor TBasicResponseError.Create(const AMessage: string);
begin
  inherited Create();
  Body.Message := AMessage;
end;

end.
