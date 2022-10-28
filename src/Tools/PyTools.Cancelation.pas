(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyTools.Cancelation'                                    *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality: Unsubscribe from a notification                        *)
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
    procedure CheckCancelled();
    function GetCancelled(): boolean;

    property IsCancelled: boolean read GetCancelled;
  end;

  TCancelation = class(TInterfacedObject, ICancelation)
  private
    FCancelled: boolean;
    function GetCancelled(): boolean;
  public
    procedure Cancel();
    procedure CheckCancelled();
  end;

implementation

{ TCancelation }

procedure TCancelation.Cancel;
begin
  FCancelled := true;
end;

function TCancelation.GetCancelled: boolean;
begin
  Result := FCancelled;
end;

procedure TCancelation.CheckCancelled;
begin
  if FCancelled then
    raise EOperationCancelled.CreateRes(@SOperationCancelled);
end;

end.
