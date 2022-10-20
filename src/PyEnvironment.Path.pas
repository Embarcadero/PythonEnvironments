(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyCommon.Path'                                          *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyCommon Resolve Paths                                *)
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
unit PyEnvironment.Path;

interface

type
  TPyEnvironmentPath = class
  public const
    ENVIRONMENT_PATH = '$(ROOT_PATH)';
    EMBEDDABLES_PATH = '$(EMBEDDABLES_PATH)';
    PYTHON_VER = '$(PYTHON_VER)';
  public
    class function CreateEmbeddablesPath(const APythonVer: boolean = false): string;
    class function CreateEnvironmentPath(const APythonVer: boolean = true): string;
    /// <summary>
    /// This function might resolve path variables, relative paths and whatever regarding paths
    /// </summary>
    class function ResolvePath(const APath: string): string; overload; static;
    class function ResolvePath(const APath, APythonVersion: string): string; overload; static;
  end;

implementation

uses
  System.IOUtils, System.SysUtils, System.RegularExpressions;

{ TPyEnvironmentPath }

class function TPyEnvironmentPath.CreateEmbeddablesPath(
  const APythonVer: boolean): string;
begin
  Result := EMBEDDABLES_PATH;
  if APythonVer then
    Result := TPath.Combine(Result, PYTHON_VER);
end;

class function TPyEnvironmentPath.CreateEnvironmentPath(
  const APythonVer: boolean): string;
begin
  Result := ENVIRONMENT_PATH;
  if APythonVer then
    Result := TPath.Combine(Result, PYTHON_VER);
end;

class function TPyEnvironmentPath.ResolvePath(const APath: string): string;

  function GetRootPath(): string;
  begin
    {$IFDEF ANDROID}
    Result := TPath.GetDocumentsPath();
    {$ELSE}
    Result := TPath.GetDirectoryName(GetModuleName(HInstance));
    {$ENDIF}
  end;

begin
  //Fix \ or / as dir separator. It varies in different platforms
  Result := TRegEx.Replace(APath, '[\/]', TPath.DirectorySeparatorChar);
  //Replace the DEPLOY_PATH variable with the app root path
  Result := Result.Replace(ENVIRONMENT_PATH, GetRootPath());
  //Replace the EMBEDDABLES_PATH variable with the platform specific path
  {$IFDEF ANDROID}
  Result := Result.Replace(EMBEDDABLES_PATH, TPath.GetDocumentsPath());
  {$ELSEIF DEFINED(MACOS)}
  Result := Result.Replace(
    EMBEDDABLES_PATH,
    TPath.Combine(
      TDirectory.GetParent(TPath.GetDirectoryName(GetModuleName(HInstance))),
      'Resources'));
  {$ELSE}
  Result := Result.Replace(EMBEDDABLES_PATH, GetRootPath());
  {$ENDIF}

  //Relative path, maybe!?
  if (Result <> ExpandFileName(Result)) then
    Result := TPath.Combine(GetRootPath(), Result);
end;

class function TPyEnvironmentPath.ResolvePath(const APath,
  APythonVersion: string): string;
begin
  Result := APath.Replace(PYTHON_VER, APythonVersion);
  Result := ResolvePath(Result);
end;

end.
