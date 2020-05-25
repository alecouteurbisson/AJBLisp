////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// Listener.cpp
//
// The listener and interpreter thread
////////////////////////////////////////////////////////////////////////////
#pragma hdrstop
#include <vcl.h>
#include <Clipbrd.hpp>

#include "Listener.h"
#include "Window.h"
#include "locale.h"
#include "ajblisp.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

TformListener *formListener;
void          *ListenerHandle;

char          *outbuf;         // Output buffer
char          *linebuf;        // Input buffer

TLispThread   *LispThread;     // The interpreter thread
bool           InputEnable;    // Enables input to listener
int            InputStart;     // Start of current input
TEvent        *LineAvailable;  // Event signalled to terminal.cpp

const int      HISTSIZE = 25;  // Size of history buffer
TStringList   *History;        // Command line history
int            HistoryIdx;     // Command line history pointer
char          *Prefix;         // Partial input prompted
bool           status = false;
char          *shared_linebuf;

//---------------------------------------------------------------------------
// This array of closures is the dispatch table for VCL access
LispThreadMethod *fun;

//---------------------------------------------------------------------------
// Start up the Listener window and then
// The interpreter thread
__fastcall TformListener::TformListener(TComponent* Owner)
  : TForm(Owner)
{
  // Set the default locale
  setlocale(LC_ALL, "");

  // Set the caption text
  char* cap = new char[24 + strlen(Version)];
  strcpy(cap, "AJBLisp");
  strcat(cap, Version);
  strcat(cap, " -  Listener");
  Caption = cap;
  delete [] cap;

  Closing = false;

  // Initialise history
  History = new TStringList();
  History->Add("");
  ListenerHandle = Terminal->Handle;

  // Fire up the interpreter thread
  LispThread = new TLispThread();
}

//---------------------------------------------------------------------------
// We get here in one of two ways
// 1) Fatal error or intentional exit from the interpreter
// 2) User pressed the close button on the listener
// The thread that receives the exit notification must ensure that the
// other thread terminates.
void __fastcall TformListener::FormClose(TObject *, TCloseAction &)
{
  // Lisp thread may have already terminated (case 1 above)
  // in which case these three lines essentially have no effect
  LispThread->Terminate();
  // Kick the thread out of its hidey-hole
  LispThread->LineAvailable->SetEvent();
  LispThread->WaitFor();

  delete LispThread;
  delete History;
}

//---------------------------------------------------------------------------
// Create an active thread that runs TLispThread::Execute() below
TLispThread::TLispThread() : TThread(false)
{
  LineAvailable = new TSimpleEvent();
  InputEnable = false;

  // Allocate and populate the VCL function dispatch table
  fun = new LispThreadMethod[ID_GFont + 1];

  fun[ID_GetLine]     = &GetLine;
  fun[ID_GWindow]     = &GWindow;
  fun[ID_GClose]      = &GClose;
  fun[ID_GClear]      = &GClear;
  fun[ID_GMove]       = &GMove;
  fun[ID_GPos]        = &GPos;
  fun[ID_GDraw]       = &GDraw;
  fun[ID_GPoint]      = &GPoint;
  fun[ID_GPick]       = &GPick;
  fun[ID_GEllipse]    = &GEllipse;
  fun[ID_GRectangle]  = &GRectangle;
  fun[ID_GText]       = &GText;
  fun[ID_GTextSize]   = &GTextSize;
  fun[ID_GPen]        = &GPen;
  fun[ID_GBrush]      = &GBrush;
  fun[ID_GFont]       = &GFont;
}

TLispThread::~TLispThread()
{
  delete [] fun;
  delete LineAvailable;
}

// Run the interpreter core and close the listener
// when the interpreter terminates
void __fastcall TLispThread::Execute()
{
  // Fire up the interpreter thread and don't return
  // until we are done
  AJBLisp(_argc, _argv);

  // Close down the listener in a way that doesn't fail
  // if it is closing already
  Synchronize((TThreadMethod)&Application->Terminate);
}

//---------------------------------------------------------------------------
// Call a function that, in turn, calls into the VCL without upsetting
// the main VCL thread
void __fastcall TLispThread::VCLCall(int func_id)
{
  Synchronize(fun[func_id]);
}

// This is just a global access point for the above that avoids
// making LispThread globally accessable
void VCLCall(int func_id)
{
  LispThread->VCLCall(func_id);
}

//---------------------------------------------------------------------------
// Listener interface functions

// This function is called via VCLCall
void __fastcall TLispThread::GetLine()
{
  InputEnable = true;
  formListener->Terminal->SelStart = LAST;
  InputStart = formListener->Terminal->SelStart;
  formListener->Terminal->ReadOnly = false;
}

// These are global entry points for LispThread functions that avoid
// making LispThread globally accessable

// Thread-safe function to read terminal line
bool ThreadWaitForLineAvailable()
{
  LispThread->LineAvailable->WaitFor(INFINITE);
  LispThread->LineAvailable->ResetEvent();
  return LispThread->CheckTerminate();
}

// Check Terminate flag
bool ThreadCheckTerminate()
{
  return LispThread->CheckTerminate();
}

//---------------------------------------------------------------------------
// Process (and veto) extended keys here
void __fastcall TformListener::TerminalKeyDown(TObject *, WORD &Key,
      TShiftState Shift)
{
  int caretloc = (Terminal->SelStart - InputStart);

  StatusBar1->Panels->Items[0]->Text = caretloc < 0 ? "HISTORY" : "";

  Terminal->ReadOnly = !InputEnable;
  // Ignore Ctrl input
  // Ctrl-C, Ctrl-X and Ctrl-V are handled automatically by TMemo
  if(Shift.Contains(ssCtrl))
    return;

  if(!InputEnable)
  {
    Key = 0;
    return;
  }

  switch(Key)
  {
    case VK_RETURN:
      // Process a complete input line
      Terminal->SelStart = InputStart;
      Terminal->SelLength = LAST;
      Terminal->GetSelTextBuf(shared_linebuf, 1022);
      if(strcmp(shared_linebuf, ""))   // if not empty!
      {
        // Operating directly on History->Strings[0]
        // doesn't work!
        AnsiString line = Prefix ? Prefix : "";
        line += shared_linebuf;
        History->Strings[0] = line;

        // Trim history buffer if necessary
        if(History->Count > HISTSIZE)
          History->Delete(History->Count - 1);

        // A new blank line to play with
        History->Insert(0, "");
        HistoryIdx = 0;

        // Write the input to the logfile, if open
        if(logfile)
          fprintf(logfile, "\n> %s\n", line.c_str());
      }
      Terminal->Lines->Add("");
      Terminal->SelStart = LAST;
      Terminal->ReadOnly = true;
      InputEnable = false;
      strcat(shared_linebuf, " ");

      // Notify the interpreter
      LispThread->LineAvailable->SetEvent();

      SendMessage(ListenerHandle, EM_SCROLLCARET, 0, 0);
      StatusBar1->Panels->Items[0]->Text = "";
    return;

    // Go back through history
    case VK_UP:
      Key = 0;
      Terminal->SelStart = InputStart;
      Terminal->SelLength = LAST;
      if(HistoryIdx == 0)
        History->Strings[0] = Terminal->SelText;
      if(HistoryIdx >= History->Count - 1)
      {
        Terminal->SelStart = LAST;
        return;
      }
      HistoryIdx++;
      Terminal->SelText = History->Strings[HistoryIdx];
      Terminal->SelStart = LAST;
      SendMessage(ListenerHandle, EM_SCROLLCARET, 0, 0);
      StatusBar1->Panels->Items[0]->Text = "";

    return;

    // Go forward through history
    case VK_DOWN:
      Key = 0;
      if(HistoryIdx == 0) return;
      HistoryIdx--;
      Terminal->SelStart = InputStart;
      Terminal->SelLength = LAST;
      Terminal->SelText = History->Strings[HistoryIdx];
      Terminal->SelStart = LAST;
      SendMessage(ListenerHandle, EM_SCROLLCARET, 0, 0);
      StatusBar1->Panels->Items[0]->Text = "";

    return;

    case VK_LEFT:
      // User can move around history but cannot
      // back up into it.
      if(caretloc == 0)
        Key = 0;
    return;

    case VK_RIGHT:
      // Moving out of history area?
      StatusBar1->Panels->Items[0]->Text = caretloc < -1 ? "HISTORY" : "";
    return;

    case VK_HOME:
      Key = 0;
      Terminal->SelStart = InputStart;
      // Make the caret visible
      SendMessage(ListenerHandle, EM_SCROLLCARET, 0, 0);
      StatusBar1->Panels->Items[0]->Text = "";

    return;

    case VK_END:
      Key = 0;
      Terminal->SelStart = LAST;
      // Make the caret visible
      SendMessage(ListenerHandle, EM_SCROLLCARET, 0, 0);
    return;

    case VK_DELETE:
      if(caretloc < 0)
        Key = 0;
    return;
  }
}

//---------------------------------------------------------------------------
// Process (and veto) character keys here
void __fastcall TformListener::TerminalKeyPress(TObject *, char &Key)
{
  int caretloc = (Terminal->SelStart - InputStart);

  StatusBar1->Panels->Items[0]->Text = caretloc < 0 ? "HISTORY" : "";

  switch(Key)
  {
    case VK_ESCAPE:
      Key = 0;
      UserBreak = true;
    return;


    case CTRLD:
      // Ensure that EOF gets through
    case CTRLC:
      // Copying is always OK
    return;

    case CTRLX:
      // Weaken Cut to Copy in history area
      if(caretloc < 0)
        Key = CTRLC;
    return;

    case VK_BACK:
      if(caretloc <= 0)
      {
        Key = 0;
        return;
      }
    break;
  }

  // Everything else jumps the cursor to the end of the input line
  if(caretloc < 0)
  {
    Terminal->SelStart = InputStart;
    Terminal->SelLength = LAST;
  }

  Terminal->ReadOnly = !InputEnable;

  if(HistoryIdx != 0)
  {
    // Modifying a history line makes it current
    int here = Terminal->SelStart;
    Terminal->SelStart = InputStart;
    Terminal->SelLength = LAST;
    HistoryIdx = 0;
    History->Strings[HistoryIdx] = Terminal->SelText;
    Terminal->SelStart = here;
  }
}

//---------------------------------------------------------------------------
void __fastcall TformListener::TerminalMouseUp(TObject *, TMouseButton,
                                               TShiftState, int, int)
{
   bool ro = Terminal->SelStart < InputStart;
   Terminal->ReadOnly = ro;
   StatusBar1->Panels->Items[0]->Text = ro ? "HISTORY" : "";
}

// Listener context menu
void __fastcall TformListener::MenuPopup(TObject *)
{
  miUndo->Enabled = Terminal->CanUndo;
  miPastePath->Enabled = Clipboard()->HasFormat(CF_TEXT);
  miPaste->Enabled = miPastePath->Enabled;
  miCopy->Enabled = Terminal->SelLength > 0;
  miCut->Enabled = miCopy->Enabled && !Terminal->ReadOnly;
  miDelete->Enabled = miCut->Enabled;
  if(Terminal->ReadOnly)
  {
    miPaste->Caption = "Paste At End";
    miPastePath->Caption = "Paste Path At End";
  }
  else
  {
    miPaste->Caption = "Paste";
    miPastePath->Caption = "Paste Path";
  }
}

// Easily paste a copied path as a string
void __fastcall TformListener::PastePath()
{
  // Add quotes and double up slashes moving backwards through buffer
  char clip[256];
  Clipboard()->GetTextBuf(clip, 256);
  char* from = clip + strlen(clip);
  char* to = clip + 256;

  *--to = '\0';
  *--to = '"';
  while((to > clip) && (from > clip))
  {
    *--to = *--from;
    if((*to == '"') || (*to == '\\'))
      *--to = '\\';
  }
  if(--to == clip) // No room for quote
  {
    ListenerStatus("Can't convert to path");
    return;
  }
  *to = '"';
  Terminal->SetSelTextBuf(to);
}

void __fastcall TformListener::ListenerMenuClick(TObject *Sender)
{
  AnsiString action = ((TMenuItem*)Sender)->Caption;

  if(action == "Undo")
    Terminal->Undo();

  else if(action == "Cut")
    Terminal->CutToClipboard();

  else if(action == "Copy")
    Terminal->CopyToClipboard();

  else if(action == "Paste")
    Terminal->PasteFromClipboard();

  else if(action == "Paste Path")
    PastePath();

  else if(action == "Paste At End")
  {
    Terminal->SelStart = LAST;
    StatusBar1->Panels->Items[0]->Text = "";
    Terminal->ReadOnly = false;
    Terminal->PasteFromClipboard();
  }

  else if(action == "Paste Path At End")
  {
    Terminal->SelStart = LAST;
    StatusBar1->Panels->Items[0]->Text = "";
    Terminal->ReadOnly = false;
    PastePath();
  }

  else if(action == "Select All")
    Terminal->SelectAll();
}


//---------------------------------------------------------------------------
// Live update of status line
void __fastcall TformListener::Timer1Timer(TObject *)
{
  if(!status) return;
  StatusBar1->Panels->Items[1]->Text = "GC: "   + IntToStr(GCCount);
  StatusBar1->Panels->Items[2]->Text = "Eval: " + IntToStr(EvalCount);
  StatusBar1->Panels->Items[3]->Text = "Cons: " + IntToStr(ConsCount);
  StatusBar1->Panels->Items[4]->Text = "Free: " + IntToStr(hlimit - hfree);
}

//---------------------------------------------------------------------------
// Toggle live update of status line
void __fastcall TformListener::StatusBar1DblClick(TObject *)
{
  Timer1->Enabled = false;
  Application->ProcessMessages();

  status = !status;

  StatusBar1->Panels->Clear();

  int n = status ? 6 : 2;
  for(int i = 0; i < n; i++)
  {
    StatusBar1->Panels->Add();
    StatusBar1->Panels->Items[i]->Width = 100;
  }
  StatusBar1->Panels->Items[n - 1]->Width = 999;

  Timer1Timer(NULL);
  StatusBar1->Panels->Items[0]->Text = Terminal->SelStart < InputStart ?
                                       "HISTORY" :
                                       "";
  Timer1->Enabled = status;
}

//---------------------------------------------------------------------------
// Write to status line
void  ListenerStatus(char* s)
{
  formListener->StatusBar1->Panels->Items[status ? 5 : 1]->Text = s;
}

//---------------------------------------------------------------------------
// Graphics window functions
//---------------------------------------------------------------------------
// Shared is the structure of shared data for LispThread -> VCL communication
// The synchronised calls completely arbitrate and serialise all access to this
// data.
Shared s;

//---------------------------------------------------------------------------
// These methods actually implement (or delegate) the graphics function of
// the same name in gfunc.cpp.

// Open a new graphics window
void __fastcall TLispThread::GWindow()
{
  s.Win = (int) new TformGWin(formListener, s.x, s.y, s.Text);
  if(s.Win)
  {
    // Return the actual size in case the user entered something silly
    s.x = IMAGE->Width;
    s.y = IMAGE->Height;
  }
}

// Close a graphics window
void __fastcall TLispThread::GClose()
{
  ((TformGWin*)s.Win)->Close();
}

// Clear a graphics window to the current brush colour
// or any specified colour
void __fastcall TLispThread::GClear()
{
  if(s.Colour != -1)
  {
    TColor tmp = CANVAS->Brush->Color;
    CANVAS->Brush->Color = (TColor)s.Colour;
    CANVAS->FillRect(IMAGE->ClientRect);
    CANVAS->Brush->Color = tmp;
  }
  else
  {
    CANVAS->FillRect(IMAGE->ClientRect);
  }
}

// Move the graphics cursor
void __fastcall TLispThread::GMove()
{
  CANVAS->MoveTo(s.x, s.y);
}

// Get the graphics cursor location
void __fastcall TLispThread::GPos()
{
  TPoint p = CANVAS->PenPos;
  s.x = p.x;
  s.y = p.y;
}

// Move the graphics cursor and draw a line between its old
// and new positions
void __fastcall TLispThread::GDraw()
{
  CANVAS->LineTo(s.x, s.y);
}

// Colour a single pixel
void __fastcall TLispThread::GPoint()
{
  CANVAS->Pixels[s.x][s.y] = (TColor)s.Colour;
}

// Read a pixel from the window
void __fastcall TLispThread::GPick()
{
  s.Colour = CANVAS->Pixels[s.x][s.y];
}

// Render an ellipse
void __fastcall TLispThread::GEllipse()
{
  CANVAS->Ellipse(s.x, s.y, s.u, s.v);
}

// Render an axis-aligned rectangle
void __fastcall TLispThread::GRectangle()
{
  CANVAS->Rectangle(s.x, s.y, s.u, s.v);
}

// Render text
void __fastcall TLispThread::GText()
{
  CANVAS->TextOut(s.x, s.y, (System::AnsiString)s.Text);
}

// Determine the extent of rendered text without rendering
void __fastcall TLispThread::GTextSize()
{
  TSize sz = CANVAS->TextExtent((System::AnsiString)s.Text);
  s.x = sz.cx;
  s.y = sz.cy;
}

// Set the window's pen
void __fastcall TLispThread::GPen()
{
  TPen* p = CANVAS->Pen;
  p->Color = (TColor)s.Colour;
  // Lines may be wide or styled so...
  if(s.Style > 0)   // ....this is a line width...
  {
    p->Width = s.Style;
    p->Style = (TPenStyle)0;
  }
  else       // ...and <= 0 indicates a line style
  {
    p->Width = 1;
    p->Style = (TPenStyle)-s.Style;
  }
}

// Set the window's brush
void __fastcall TLispThread::GBrush()
{
  TBrush* b = CANVAS->Brush;
  b->Color = (TColor)s.Colour;
  b->Style = (TBrushStyle)s.Style;
}

// Set the window's font
void __fastcall TLispThread::GFont()
{
  TFont* f = CANVAS->Font;
  f->Charset = DEFAULT_CHARSET;
  f->Name = (System::AnsiString)s.Text;

  if(s.x > 0) f->Height = s.x;
  else        f->Size = -s.x;

  f->Style.Clear();
  if(s.Style & 1) f->Style << fsBold;
  if(s.Style & 2) f->Style << fsItalic;
  if(s.Style & 4) f->Style << fsUnderline;
  if(s.Style & 8) f->Style << fsStrikeOut;

  f->Pitch = (TFontPitch)s.y;

  if(s.Colour != -1)
    f->Color = (TColor)s.Colour;
}

//---------------------------------------------------------------------------

//---------------------------------------------------------------------------

