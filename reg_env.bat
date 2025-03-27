@echo off

set PRODVER=22.0
set PYENVPATH=%cd%

if not "%~1"=="" (SET PRODVER=%~1)
if not "%~2"=="" (SET PYENVPATH=%~2)

//A trick to solve relative path issues
pushd %PYENVPATH%
set PYENVPATH=%cd%
popd

reg add "HKEY_CURRENT_USER\Software\Embarcadero\BDS\%PRODVER%\Environment Variables" /v PYTHONENVIRONMENTDIR /d "%PYENVPATH%" /f