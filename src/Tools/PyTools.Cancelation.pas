unit PyTools.Cancelation;

interface

uses
  System.SysUtils,
  System.SysConst,
  System.Generics.Collections;

type
  ICancelation = interface
    ['{749AF528-3209-4F5F-9CAA-58048285C275}']
    procedure Cancel();
    procedure CheckCanceled();
    function IsCanceled(): boolean;
  end;

  TCancelation = class(TInterfacedObject, ICancelation)
  private
    FCanceled: boolean;
  public
    procedure Cancel();
    procedure CheckCanceled();
    function IsCanceled(): boolean;
  end;

implementation

{ TCancelation }

procedure TCancelation.Cancel;
begin
  FCanceled := true;
end;

function TCancelation.IsCanceled: boolean;
begin
  Result := FCanceled;
end;

procedure TCancelation.CheckCanceled;
begin
  if FCanceled then
    raise EOperationCancelled.CreateRes(@SOperationCancelled);
end;

end.
