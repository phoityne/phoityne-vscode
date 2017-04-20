
20170402 phoityne-vscode-0.0.13.0
  * [FIX] [10](https://github.com/phoityne/phoityne-vscode/issues/10) : Unable to set breakpoints
  * [FIX] [11](https://github.com/phoityne/phoityne-vscode/issues/11) : Debugger not progressing past IO
  * [ADD] [12](https://github.com/phoityne/phoityne-vscode/issues/12) : support Column break points
  * [MODIFY] [6](https://github.com/phoityne/phoityne-vscode/issues/6) : adding stopOnEntry setting on launch.json 
  * [MODIFY] : add ghci-options -fprint-evld-with-show to stack run command in launch.json file.

20170205 phoityne-vscode-0.0.12.0
  * [FIX] [8](https://github.com/phoityne/phoityne-vscode/issues/8) : Unterminated process after leaving GHCi 


20161218 phoityne-vscode-0.0.11.0
  * [ADD] Hit count break conditionに対応した。
  * [FIX] エラー終了時にterminate eventを送信するように修正した。 


20161009 phoityne-vscode-0.0.10.0
  * [MODIFY] REPL結果をイベントで返すように変更した。EvaluateResponseで返した場合は、複数行表示ができないため。 
  * [MODIFY] REPLにおいて、複数行の入力に対応した。 
  * [MODIFY] REPLにおいて、デバッグ関連のコマンドを実行しないように変更した。 
  * [FIX] バインディング変数情報の取得において、複数行にまたがる情報のパースが失敗していた箇所を修正した。 


20160919 phoityne-vscode-0.0.9.0
  * [ADD] ConfigurationDoneRequestに対応した。
  * [ADD] CompletionsRequestに対応した。
  * [MODIFY] Hover時の型表示を改善した。
  * [MODIFY] Hover時のforce実行を止めた。
  * [FIX] setBreakpointsResponse, setFunctionBreakpointsResponseのBodyデータにおいて、キー名を修正した。


20160804 phoityne-vscode-0.0.8.0
  * [MODIFY] モジュール構成の変更。
  * [MODIFY] debugger adopter interface Capabilitiesの変更に対応した。


20160704 phoityne-vscode-0.0.7.0
  * [MODIFY] debugger adopter interface Capabilitiesの変更に対応した。
  * [MODIFY] debugger adopter interface StackFrameの変更に対応した。(endLin, endColumnの追加)
  * [MODIFY] debugger adopter interface Variableの変更に対応した。(typeの追加)
  

20160626 phoityne-vscode-0.0.6.0

  * [MODIFY] launchリクエストのghciCmdパラメータで指定したghci起動コマンドを使用するように変更した。
  * [MODIFY] ghciの起動に失敗した場合、デバッグを終了するように修正した。


20160605 phoityne-vscode-0.0.5.0

  * [MODIFY] launchリクエストのnoDebugパラメータをMaybeに変更した。


20160601 phoityne-vscode-0.0.4.0

  * [MODIFY] スタックトレースの表示を番号から関数名に変更した。
  * [MODIFY] GHC-8.0.1のghciにおいて、:step結果の出力変更(Stopped in)に対応した。


20160515 phoityne-vscode-0.0.3.0

  * [ADD] tasks.jsonが存在しない場合は、作成するようにした。
  * [ADD] package.json にキーバインディング設定を追加した。(stack build, stack clean, stack test, stack watch)
  * [ADD] hover requestに対して、:info結果を返すようにした。


20160508 phoityne-vscode-0.0.2.0

  * [ADD] デバッガ起動後、ファイル保存時にghciにリロードするようにした。
  * [ADD] 条件付きブレークポイントに対応した。


20160504 phoityne-vscode-0.0.1.0

  * [INFO] Initial release.


