@echo off

set where_cmd=where $path:phoityne-vscode.exe
set pohityne_path=
for /f "usebackq delims=" %%a in (`%where_cmd%`) do set pohityne_path=%%a

if "%pohityne_path%" equ "" (
  echo "Content-Length: 199"
  echo.
  echo {"command":"initialize","success":false,"request_seq":1,"seq":1,"type":"response","message":"phoityne-vscode.exe is not found. Run 'stack install phoityne-vscode', and put it to PATH environment."}
  exit 1
)


phoityne-vscode.exe --hackage-version=0.0.19.0

