(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.DownloadBundle'               *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality: Helpers for the Python project menu                    *)
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
unit PyEnvironment.Project.IDE.DownloadBundle;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.Net.HttpClient,
  PyEnvironment.Project.IDE.Types;

type
  TDownloadProgress = TProc<integer>;
  TDownloadPythonBundle = class
  public
    class function Download(const ABundleName, AFileName: string;
      AProgress: TDownloadProgress): TDeployTaskOutput; static;
  end;

  EDownloadBundleFailed = class(Exception);

implementation

uses
  System.IOUtils,
  System.Math;

{ TDownloadPythonBundle }

class function TDownloadPythonBundle.Download(
  const ABundleName, AFileName: string; AProgress: TDownloadProgress): TDeployTaskOutput;
const
  DEFAULT_URL = 'https://embt.sfo3.cdn.digitaloceanspaces.com/';
var
  LNetClient: THTTPClient;
  LStream: TFileStream;
  LLink: string;
  LResponse: IHTTPResponse;
  LLastPercentage: integer;
begin
  LLink := DEFAULT_URL + ABundleName;

  LNetClient := THTTPClient.Create();
  try
    LLastPercentage := 0;
    LNetClient.ReceiveDataCallBack :=
    procedure(const Sender: TObject;
      AContentLength, AReadCount: Int64; var AAbort: Boolean)
    var
      LPercentage: integer;
    begin
      LPercentage := Integer(Trunc((AReadCount / AContentLength) * 100));
      if LPercentage <> LLastPercentage then begin
        LLastPercentage := LPercentage;
        AProgress(LPercentage);
      end;
    end;

    try
      LStream := TFileStream.Create(AFileName, fmCreate or fmOpenReadWrite);
      try
        LResponse := LNetClient.Get(LLink, LStream);

        Result := ((LResponse.StatusCode div 100) = 2); // Accepts any 200

        if Result.Success then begin
          // Success response
          Result.Description := 'Success';
          Result.Text := 'File successfully downloaded.';
        end else if not LResponse.StatusText.IsEmpty() then begin
          // Failed with status text
          Result.Description := 'Failed';
          Result.Text := LResponse.StatusText;
        end else begin
          // Failed without status text
          Result.Description := 'Failed';
          Result.Text := 'Try again.';
        end;
      finally
        LStream.Free();
      end;
    except
      on E: Exception do begin
        TFile.Delete(AFileName);
        Result := false;
        Result.Description := 'Error';
        Result.Text := E.Message;
      end;
    end;
  finally
    LNetClient.Free();
  end;
end;

end.
