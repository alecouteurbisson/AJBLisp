////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2006
//
////////////////////////////////////////////////////////////////////////////
// ListenerIf.h
//
// Global listener interface header
////////////////////////////////////////////////////////////////////////////
#ifndef ListenerifH
#define ListenerifH

extern bool  ThreadWaitForLineAvailable();   // Thread synchronisation
extern bool  ThreadCheckTerminate();         // Should we die now?

extern void *ListenerHandle;                 // Window handle for text output
extern char *Prefix;                         // Prefix for History
extern void  ListenerStatus(char* s);        // Write to status line

extern char *shared_linebuf;                 // Shared variable for VCL access
//---------------------------------------------------------------------------

// These functions must be called by ID because putting TLispThread
// in here (with the required VCL headers) breaks the compiler.
// Therefore we have no reference to TLispThread here.
enum
{
  ID_GetLine,
  ID_GWindow,     ID_GClose,      ID_GClear,
  ID_GMove,       ID_GPos,        ID_GDraw,
  ID_GPoint,      ID_GPick,       ID_GEllipse,
  ID_GRectangle,  ID_GText,       ID_GTextSize,
  ID_GPen,        ID_GBrush,      ID_GFont
};


// Macro to create VCL access functions
#define VCLCALL(func) \
inline void Listener##func() \
{ VCLCall(ID_##func); }

// Call one of the above functions
extern void VCLCall(int func_id);

// Thread-safe function to read terminal line
VCLCALL(GetLine);      // Define ListenerGetLine()

////////////////////////////////////////////////////////////////////////////
// Graphics interface
////////////////////////////////////////////////////////////////////////////
// Shared data to communicate with VCL thread
struct Shared
{
  int    x, y, z;      // Coordinates
  int    u, v, w;      // Secondary coordinates / dimensions
  char  *Text;         // Captions, text
  int    Win;          // Window handle
  int    Style;        // Pen/Brush Style
  int    Colour;       // Colour
};

extern Shared s;

// Easy reference to graphics canvas and image
#define IMAGE ((TformGWin*)s.Win)->Image
#define CANVAS IMAGE->Canvas

// Declare inline function to handle VCL access
// These functions call into the VCL via the Synchronize() method
// to ensure proper arbitration of shared data.
VCLCALL(GWindow);
VCLCALL(GClose);
VCLCALL(GClear);
VCLCALL(GMove);
VCLCALL(GPos)
VCLCALL(GDraw);
VCLCALL(GPoint);
VCLCALL(GPick);
VCLCALL(GEllipse);
VCLCALL(GRectangle);
VCLCALL(GText);
VCLCALL(GTextSize);
VCLCALL(GPen);
VCLCALL(GBrush);
VCLCALL(GFont);

const int LAST = 0x7FFF;   // Last character in Terminal buffer

// Enumerate active control key values
enum { CTRLC = 'C' & 0x1F, CTRLX = 'X' & 0x1F,
       CTRLV = 'V' & 0x1F, CTRLD = 'D' & 0x1F };
#endif
