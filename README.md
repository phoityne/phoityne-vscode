

# Phoityne VSCode

Phoityne is a ghci debug viewer for Visual Studio Code.


## Information

* [2017/04/02] phoityne-vscode released.  
  * Marketplace [phoityne-vscode-0.0.11](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)
  * hackage [phoityne-vscode-0.0.13.0](https://hackage.haskell.org/package/phoityne-vscode)  
* Release Summary
  * FIX [10](https://github.com/phoityne/phoityne-vscode/issues/10) : Unable to set breakpoints
  * FIX [11](https://github.com/phoityne/phoityne-vscode/issues/11) : Debugger not progressing past IO
  * ADD [12](https://github.com/phoityne/phoityne-vscode/issues/12) : support Column breakpoints
  * MODIFY [6](https://github.com/phoityne/phoityne-vscode/issues/6) : add "stopOnEntry" setting to launch.json 
  * MODIFY : add "--ghci-options -fprint-evld-with-show" to ghci command option in launch.json, instead of hard-coded.



![01_debug.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/01_debug.gif)


## Important

* Limitation. Breakpoint can be set in a .hs file which defineds "module ... where".
* Limitation. Source file extension must be ".hs"
* Limitation. Can not use STDIN handle while debugging. 
* When you start debugging for the first time, .vscode/tasks.json will be created automatically. Then you can use F6, F7, F8 shortcut key.
  * F5 : start debug
  * F6 : show command menu (for stack watch)
  * Shift + F6 : stop stack watch
  * F7 : stack clean & build
  * F8 : stack test
  * F9 : put bp on current line
  * Shift + F9 : put bp on current column
* While debugging, you can use F5, F9, F10, F11 shortcut key.
  * F5 : jump to next bp
  * F9 : put bp on the line
  * Shift + F9 : put bp on the column
  * F10 : step next
  * F11 : step into


## Install


### Run stack install

    % stack install phoityne-vscode
      . . . . .
    %

Add 'phoityne-vscode.exe' to PATH environment.

    % where $path:phoityne-vscode.exe
    C:\Users\[user name]\AppData\Roaming\local\bin\phoityne-vscode.exe
    
    % phoityne-vscode --version
    phoityne-vscode-0.0.13.0
    %
    % code



### Install vscode extensions

1. run VSCode and open stack project __Folder__ from file menu. 
2. open Extensions from side menu of VSCode.
3. search "haskell" 
4. select "[__Haskell GHCi debug viewer Phoityne__](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)"

  
  
## Features

### Run to Cursor

![03_run_to_cursor.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/03_run_to_cursor.gif)


### Bindings & Watch

The variable added to watch will be forced.

![02_watch.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/02_watch.gif)


### Stack trace

![05_stacktrace.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/05_stacktrace.gif)


### Break condition

![04_condition.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/04_condition.gif)

### Hit count break condition

Supports these operators.
*  ==
*  /=
*  <, >
*  <=, >=
*  mod, %
*  just digit is same with '>='

![07_hit_count.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/07_hit_count.gif)

### Repl & Completions

![06_repl.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/06_repl.gif)

## Capabilites

* supportsConfigurationDoneRequest : yes
* supportsFunctionBreakpoints : yes
* supportsConditionalBreakpoints : yes
* supportsHitConditionalBreakpoints : yes
* supportsEvaluateForHovers : yes
* exceptionBreakpointFilters : no
* supportsStepBack : no
* supportsSetVariable : no
* supportsRestartFrame : no
* supportsGotoTargetsRequest : no
* supportsStepInTargetsRequest : no
* supportsCompletionsRequest : yes
* supportsModulesRequest : no
* additionalModuleColumns : no
* supportedChecksumAlgorithms : no
* supportsRestartRequest : no
* supportsExceptionOptions : no
* supportsValueFormattingOptions : no
* supportsExceptionInfoRequest : no

## Configuration

### __.vscode/launch.json__

|NAME|REQUIRED OR OPTIONAL|DEFAULT SETTING|DESCRIPTION|
|:--|:--:|:--|:--|
|startup|required|${workspaceRoot}/test/Spec.hs|debug startup file, will be loaded automatically.|
|ghciCmd|required|stack ghci --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show|launch ghci command, must be Prelude module loaded. For example, "ghci -i${workspaceRoot}/src", "cabal exec -- ghci -i${workspaceRoot}/src"|
|ghciPrompt|required|H>>=|ghci command prompt string.|
|stopOnEntry|required|true|stop or not after debugger launched.
|logFile|required|${workspaceRoot}/.vscode/phoityne.log|internal log file.|
|logLevel|required|WARNING|internal log level.|

### __.vscode/tasks.json__

|TASK NAME|REQUIRED OR OPTIONAL|DEFAULT SETTING|DESCRIPTION|
|:--|:--:|:--|:--|
|stack build|required|stack build|task definition for F6 shortcut key.|
|stack clean & build|required|stack clean && stack build|task definition for F7 shortcut key.|
|stack test|required|stack test|task definition for F8 shortcut key.|
|stack watch|required|stack build --test --no-run-tests --file-watch|task definition for F6 shortcut key.|


## Version history

* [2017/02/05] phoityne-vscode released.  
  * Marketplace [phoityne-vscode-0.0.10](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)
  * hackage [phoityne-vscode-0.0.12.0](https://hackage.haskell.org/package/phoityne-vscode)  

* [2016/12/18] phoityne-vscode released.  
  * Marketplace [phoityne-vscode-0.0.9](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)
  * hackage [phoityne-vscode-0.0.11.0](https://hackage.haskell.org/package/phoityne-vscode)  

* [2016/10/09] phoityne-vscode released.  
  * Marketplace [phoityne-vscode-0.0.8](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)
  * hackage [phoityne-vscode-0.0.10.0](https://hackage.haskell.org/package/phoityne-vscode)  

* [2016/09/19] for VSCode-1.5, updated vscode extension and haskell library. Please use new versions.
  * Marketplace [phoityne-vscode-0.0.7](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)
  * hackage [phoityne-vscode-0.0.9.0](https://hackage.haskell.org/package/phoityne-vscode)  

* [2016/08/07] for VSCode-1.4, updated vscode extension and haskell library. Please use new versions.
  * Marketplace [phoityne-vscode-0.0.6](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)
  * hackage [phoityne-vscode-0.0.8.0](https://hackage.haskell.org/package/phoityne-vscode)

* [2016/07/10] for VSCode-1.3, updated vscode extension and haskell library. Please use new versions.
  * Marketplace [phoityne-vscode-0.0.5](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)
  * hackage [phoityne-vscode-0.0.6.0](https://hackage.haskell.org/package/phoityne-vscode)

* [2016/06/06] for VSCode-1.2, updated vscode extension and haskell library. Please use new versions.
  * Marketplace [phoityne-vscode-0.0.4](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)
  * hackage [phoityne-vscode-0.0.5.0](https://hackage.haskell.org/package/phoityne-vscode)
