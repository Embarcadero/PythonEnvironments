unit PyTools.Notification.Content;

interface

type
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

implementation

end.
