////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// Listener.h
//
// The listener and interpreter thread
////////////////////////////////////////////////////////////////////////////
#ifndef ListenerH
#define ListenerH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <syncobjs.hpp>
#include <dos.h>
#include <ExtCtrls.hpp>  // for _argc and _argv
#include "ajblispinit.h"
#include "repl.h"
#include "eval.h"
#include "store.h"
#include "listenerif.h"
#include <Menus.hpp>

//---------------------------------------------------------------------------

class TformListener : public TForm
{
__published:	// IDE-managed Components
  TMemo *Terminal;
  TStatusBar *StatusBar1;
  TTimer *Timer1;
        TPopupMenu *menuListener;
        TMenuItem *miUndo;
        TMenuItem *miCut;
        TMenuItem *miCopy;
        TMenuItem *miPaste;
        TMenuItem *miDelete;
        TMenuItem *miSelectAll;
        TMenuItem *miSeparator1;
        TMenuItem *miSeparator2;
        TMenuItem *miPastePath;
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall TerminalKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall TerminalMouseUp(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
  void __fastcall TerminalKeyPress(TObject *Sender, char &Key);
  void __fastcall Timer1Timer(TObject *Sender);
  void __fastcall StatusBar1DblClick(TObject *Sender);
  void __fastcall ListenerMenuClick(TObject *Sender);

  void __fastcall MenuPopup(TObject *Sender);

private:	// User declarations
  void __fastcall PastePath();
public:		// User declarations
  __fastcall TformListener(TComponent* Owner);
  bool Closing;
};

//---------------------------------------------------------------------------

extern PACKAGE TformListener *formListener;

typedef TThreadMethod LispThreadMethod;

class TLispThread : public TThread
{
public:
// Terminal function stuff
  TSimpleEvent* LineAvailable;
  TLispThread();
  ~TLispThread();

  // Call a VCL function via synchronize()
  void __fastcall VCLCall(int func_id);

  // The interpreter thread function
  void __fastcall Execute();

  // This check is made on every call to Eval and just
  // after reading a line of input.  The listener has
  // already been forced to return if waiting for input
  // in the latter case.
  bool __fastcall CheckTerminate()
  {
    return Terminated;
  }

  // Request listener input
  void __fastcall GetLine();

  // Graphic window stuff
  void __fastcall GWindow();
  void __fastcall GClose();
  void __fastcall GClear();
  void __fastcall GMove();
  void __fastcall GPos();
  void __fastcall GDraw();
  void __fastcall GPoint();
  void __fastcall GPick();
  void __fastcall GEllipse();
  void __fastcall GRectangle();
  void __fastcall GText();
  void __fastcall GTextSize();
  void __fastcall GPen();
  void __fastcall GBrush();
  void __fastcall GFont();
};

// Interpreter thread
extern TLispThread* LispThread;

// Dispatch table for VCL access functions
extern LispThreadMethod* fun;

#endif
