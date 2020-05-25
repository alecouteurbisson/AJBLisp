;NSIS Modern User Interface
;Basic Example Script
;Written by Joost Verburg

;--------------------------------
;Include Modern UI

  !include "MUI.nsh"

;--------------------------------
;General

  ;Name and file
  Name "AJBLisp"
  OutFile "SetupAJBLisp.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\AJBLisp"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKCU "Software\AJBLisp" ""

  !define LISPPATH "$INSTDIR\"

  !define MUI_ABORTWARNING
  ; !insertmacro MUI_PAGE_LICENSE "${NSISDIR}\Docs\Modern UI\License.txt"
  ;!insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_LANGUAGE "English"
  
  !include WriteEnvStr.nsh

Section "Install" 

  SetOutPath "$INSTDIR"
  
  ; Required for correct operation
  file "src\AJBLisp.exe"
  file "src\vcl50.bpl"
  file "src\Init.lsp"
  file "src\sort.lsp"
  file "src\pprint.lsp"
  file "src\ls.lsp"
 
  ; Test/example files
  file "src\Test.lsp"
  file "src\test-parse.lsp"
  file "regex.lsp"
  file "test-regex.lsp"
  file "gfxtest.lsp"
  file "priority.lsp"
  file "parse.lsp"
  file "compiler.lsp"
  file "sudoku.lsp"
  
  ; AJBLispEdit
  file "AJBLispEdit.exe"
  file "SciLexer.dll"
  file "msvcr71.dll"
  file "lisp.properties"
  file "SciTEGlobal.properties"
  
  ; Help
  file "src\AJBLisp.doc"
  
  ; Source
  SetOutPath "$INSTDIR\src\"
  file "src\*.cpp"
  file "src\*.h"
  file "src\*.dfm"
  file "src\*.res"
  file "src\help.css"
  file "src\HtmlHelp.lsp"
  file "src\errors.lsp"
  file "src\mkerr.lsp"
  file "src\AJBLisp.bpr"
  
  ;Store installation folder
  WriteRegStr HKCU "Software\AJBLisp" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  !define ReadEnvStr_RegKey \
       'HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"'

  Push LISPPATH
  Push '${LISPPATH}'
  Call WriteEnvStr

  WriteRegStr HKCR ".lsp" "" "LispSource"
  WriteRegStr HKCR ".lisp" "" "LispSource"
  WriteRegStr HKCR "LispSource" "" "Lisp Source File"
  ;WriteRegStr HKCR "LispSource\DefaultIcon" "" "$INSTDIR\src\lsp.ico"
  WriteRegStr HKCR "LispSource\shell" "" "edit"
  WriteRegStr HKCR "LispSource\shell\run" "" "Run program"
  WriteRegStr HKCR "LispSource\shell\run\command" "" '"$INSTDIR\AJBLisp.exe" -l"%1"'
  WriteRegStr HKCR "LispSource\shell\edit" "" "Edit with AJBLispEdit"
  WriteRegStr HKCR "LispSource\shell\edit\command" "" '"$INSTDIR\AJBLispEdit.exe" "%1"'
  
  ; Build help files
  Exec '"$INSTDIR\AJBLisp.exe" -l"$INSTDIR\src\HtmlHelp.lsp" "$INSTDIR"' 
  
  writeuninstaller '$EXEDIR\uninst.exe'
SectionEnd

Section "Uninstall"

  # remove the variable
  Push LISPPATH
  Call un.DeleteEnvStr

  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  DeleteRegKey /ifempty HKCU "Software\AJBLisp"
  DeleteRegKey HKCR "LispSource\shell"
SectionEnd