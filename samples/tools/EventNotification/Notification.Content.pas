unit Notification.Content;

interface

uses
  PyTools.Notification;

const
  EVENT_CHANNEL_IDENTIFIER_MESSAGE = 'message';

type
  TMessageBody = class
  public
    MessageContent: string;
  end;

  [EventType(EVENT_CHANNEL_IDENTIFIER_MESSAGE)]
  TMessageEvent = class(TEventChannel<TMessageBody>)
  public
    constructor Create(const AMessageContent: string);
  end;

implementation

{ TMessageEvent }

constructor TMessageEvent.Create(const AMessageContent: string);
begin
  inherited Create();
  Body.MessageContent := AMessageContent;
end;

end.
