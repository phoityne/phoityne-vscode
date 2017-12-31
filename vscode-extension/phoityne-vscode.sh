#!/bin/sh

export PHOITYNE_PATH=`which phoityne-vscode`

if [ "X" = "X$PHOITYNE_PATH" ]; then
  echo -e "Content-Length: 194\r\n\r"
  echo '{"command":"initialize","success":false,"request_seq":1,"seq":1,"type":"response","message":"phoityne-vscode is not found. Run `stack install phoityne-vscode`, and put it to PATH environment."}'

  exit 1
fi

phoityne-vscode --hackage-version=0.0.20.0


