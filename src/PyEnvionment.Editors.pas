unit PyEnvionment.Editors;

interface

uses
  System.SysUtils, Classes, DesignIntf, DesignEditors;

type
  TPyEnvironmentEmbeddedPythonVersionProperty = class (TStringProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register();

implementation

uses
  ToolsAPI,
  PyEnvironment.Project.IDE.Helper,
  PyEnvironment.Embeddable;

procedure Register();
begin
  RegisterPropertyEditor(TypeInfo(string), TPyEmbeddedEnvironment, 'PythonVersion', TPyEnvironmentEmbeddedPythonVersionProperty);
end;

{ TPyEnvironmentEmbeddedPythonVersionProperty }

function TPyEnvironmentEmbeddedPythonVersionProperty.GetValue: string;
var
  LProject: IOTAProject;
begin
  LProject := GetActiveProject();
  if Assigned(LProject) then begin
    if TPyEnvironmentProjectHelper.IsPyEnvironmentDefined[LProject] then
      Result := TPyEnvironmentProjectHelper.CurrentPythonVersion[LProject]
    else
      Result := inherited;
  end else
    Result := inherited;
end;

procedure TPyEnvironmentEmbeddedPythonVersionProperty.SetValue(
  const Value: string);
var
  LProject: IOTAProject;
begin
  LProject := GetActiveProject();
  if Assigned(LProject) then begin
    if not TPyEnvironmentProjectHelper.IsPyEnvironmentDefined[LProject] then
      inherited;
  end else
    inherited;
end;

end.
