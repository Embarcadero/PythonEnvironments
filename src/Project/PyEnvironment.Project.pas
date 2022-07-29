unit PyEnvironment.Project;

interface

uses
  System.Classes;

type
  TPyEnvironmentProject = class
  private
    function GetPythonVersion: string;
    function GetEnabled: boolean;
  private
    class constructor Create();
    class destructor Destory();
  public
    property Enabled: boolean read GetEnabled;
    property PythonVersion: string read GetPythonVersion;
  end;

var
  PythonProject: TPyEnvironmentProject;

implementation

uses
  System.SysUtils, System.Rtti;

{ TPyEnvironmentProject }

class constructor TPyEnvironmentProject.Create;
begin
  PythonProject := TPyEnvironmentProject.Create();
end;

class destructor TPyEnvironmentProject.Destory;
begin
  FreeAndNil(PythonProject);
end;

function TPyEnvironmentProject.GetEnabled: boolean;
begin
  {$IFDEF PYTHON}
  Result := true;
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

function TPyEnvironmentProject.GetPythonVersion: string;
begin
  {$IFDEF PYTHONVER37}
  Result := '3.7';
  {$ELSEIF DEFINED(PYTHONVER38)}
  Result := '3.8';
  {$ELSEIF DEFINED(PYTHONVER39)}
  Result := '3.9';
  {$ELSEIF DEFINED(PYTHONVER310)}
  Result := '3.10';
  {$ELSE}
  Result := String.Empty;
  {$ENDIF}
end;

end.
