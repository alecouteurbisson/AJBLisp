////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// store.h
////////////////////////////////////////////////////////////////////////////
#ifndef StoreH
#define StoreH
#pragma alignment

#include <stdio.h>

typedef unsigned char  byte;
typedef unsigned short word;
typedef unsigned long  integer;
typedef byte *pointer;

const int HASHSIZE = 379;

// Backtrace frame limit
extern int backtrace;
// GC Counter
extern  int GCCount;
// Cons node counter
extern int ConsCount;

// The allowed types of heap cell
// tRELOC cells exist only during GC and hold forwarding adresses
enum TYPE
{
  tCONS, tSYM, tCONST, tKEY, tCHR, tSTR, tINT, tREAL,
  tVECT, tCVECT, tSUBR, tSUBRL, tFSUBR, tFSUBRL, tFILE, tWIND,
  tRELOC
};

class CELL;

// The fundamental LISP datatype
typedef CELL volatile *LISP;

// Protected pointer class
// All references to heap objects must be via a protected pointer if
// GC can occur during the pointers lifetime.
class pp
{
public:
  static pp* pphead;
  LISP p;
  const pp* next;

  inline pp(LISP l) : next(pphead) { pphead = this; p = l; }
  inline ~pp() { pphead = (pp*)next; }
};

// Create an initialised protected pointer and a reference to its value
// Note that protected pointers must always point at a valid LISP object
// or nil whenever GC can occur
#define protect(ptr, init) pp protect_##ptr(init);   \
                           LISP& ptr(protect_##ptr.p);

// The following are used to typecast code pointers
// They are pointers to a function returning a lisp cell
// F/SUBRs take an arg count and an arg array
// F/SUBRLs take an arglist
// The term FSUBR simply means a SUBR that does not evaluate its arguments
typedef LISP(*SUBR) (byte, LISP[]);
typedef LISP(*FSUBR) (byte, LISP[]);
typedef LISP(*SUBRL) (LISP&);
typedef LISP(*FSUBRL) (LISP&);

// Special lisp atoms
#define NIL (LISP)0L             // Nil, nada, nothing
extern LISP UNDEFINED;           // Initial value of symbols
extern LISP T;                   // Truth
extern LISP LAMBDA;              // Function marker
extern LISP MACRO;               // Macro marker
extern LISP DOT;                 // The CONS operator
extern LISP EOF_;                // End of file
extern LISP QUOTE;               // Quote function
extern LISP SYSTEM;              // System command processor

// Not all keys and constants are listed here, only those that are
// referenced from within the interpreter

// File open mode keys
extern LISP READ;
extern LISP WRITE;
extern LISP CREATE;

// String compare keys
extern LISP CASE;
extern LISP NOCASE;
extern LISP COLLATE;

// Seek origin keys
extern LISP SK_START;
extern LISP SK_END;
extern LISP SK_REL;

// Object type keys. Prefixed with l to avoid namespace clashes
extern LISP lCONS;
extern LISP lSYMBOL;
extern LISP lCONSTANT;
extern LISP lCHARACTER;
extern LISP lKEY;
extern LISP lSTRING;
extern LISP lINTEGER;
extern LISP lFLOAT;
extern LISP lVECTOR;
extern LISP lCVECTOR;
extern LISP lSUBR;
extern LISP lSUBRL;
extern LISP lFSUBR;
extern LISP lFSUBRL;
extern LISP lFILE;
extern LISP lWINDOW;

// Stream constants
extern LISP STDIN;
extern LISP STDOUT;
extern LISP STDERR;

// History vars
extern LISP PERCENT;
extern LISP PERCENT1;
extern LISP PERCENT2;
extern LISP PERCENT3;
extern LISP PERCENT4;
extern LISP PERCENT5;
extern LISP PERCENT6;

// No object flag
// Represents no object in cases where nil and :undefined are valid
// No function should ever return :none apart from obvious exceptions
// like (symbol ":none")
// Used by (error ...)
extern LISP NONE;

// Case default
extern LISP ELSE;

// Graphics view transform
struct View2
{
  double vt[2][3];

  void zero()
  {
    vt[0][0] = 0.0;
    vt[1][0] = 0.0;
    vt[0][1] = 0.0;
    vt[1][1] = 0.0;
    vt[0][2] = 0.0;
    vt[1][2] = 0.0;
  }
};

//CELLS /////////////////////////////////////////////////////////////////////
// The various species of Lisp cell
struct CELL
{
  TYPE flags;
  byte size;
};

// Cells with a size byte of zero have their actual size stored as a long
// integer in the first four bytes of the rest of the structure (BIGCELLs).
// This allows vector data and strings, to be stored in the main heap.
struct CBIG : CELL
{
  int lsize;
};

// A cons node
struct CCONS : CELL
{
  LISP car;
  LISP cdr;
};

// A global symbol
struct CSYM : CELL
{
  LISP value;
  LISP prop;
  LISP hash;
  char name[];
};

// A character
struct CCHR : CELL
{
  char chr;
  char pad[3];     // Must leave room for forwarding pointer
};

// A string
struct CSTR : CBIG
{
  char text[];
};

// An integer
struct CINT : CELL
{
  int ivalue;
};

// A float
struct CFLT : CELL
{
  double rvalue;
};

// A vector
struct CVECT : CBIG
{
  int dim;
  LISP data[];
};

// A primitive function
struct CEXEC : CELL
{
  pointer code;
};

// A file handle
struct CFILE : CELL
{
  FILE *file;
};

// Graphics window
struct CWIND : CELL
{
  int window;
  int width, height;
  View2 view2;
};

// Relocated cell type used only during GC
struct CRELOC : CELL
{
  LISP reloc;
};

const integer CONS_SIZE = sizeof(CCONS);
const integer SYM_SIZE  = sizeof(CSYM);    // + size of name
const integer CHR_SIZE  = sizeof(CCHR);
const integer STR_SIZE  = sizeof(CSTR);    // + size of string (big cell)
const integer REAL_SIZE = sizeof(CFLT);
const integer INT_SIZE  = sizeof(CINT);
const integer VECT_SIZE = sizeof(CVECT);   // + size of data (big cell)
const integer EXEC_SIZE = sizeof(CEXEC);
const integer FILE_SIZE = sizeof(CFILE);
const integer WIND_SIZE = sizeof(CWIND);

// Member access functions.
// Note: These are not type-safe.  You must ensure that
// these functions are only applied to cells of the correct
// underlying type.
inline TYPE& flags(LISP x)
{
  return ((CELL *)x)->flags;
}

inline byte& size(LISP x)
{
  return ((CELL *)x)->size;
}

// Get size of cell known to be a bigcell
inline int & bigsize(LISP x)
{
  return ((CBIG*)x)->lsize;
}

// This function must be called to get the size of a cell if that cell
// might be a vector or a string (a bigcell).
inline int getsize(LISP x)
{
  return size(x) ? (int)size(x) : bigsize(x);
}

// These list access functions assume that the access is valid
// this must be ensured by an explicit test if there is any
// possibility of an invalid list [e.g. (cadr '(1)), (car nil) ]
inline LISP& car(LISP x)
{
  return ((CCONS *) x)->car;
}

inline LISP& cdr(LISP x)
{
  return ((CCONS *) x)->cdr;
}

inline LISP& caar(LISP x)
{
  return ((CCONS *) ((CCONS *) x)->car)->car;
}

inline LISP& cadr(LISP x)
{
  return ((CCONS *) ((CCONS *) x)->cdr)->car;
}

inline LISP& cdar(LISP x)
{
  return ((CCONS *) ((CCONS *) x)->car)->cdr;
}

inline LISP& cddr(LISP x)
{
  return ((CCONS *) ((CCONS *) x)->cdr)->cdr;
}

// These object accessor functions assume that the argument has the
// appropriate type.  This must be ensured by an explicit test if
// there is any possibility of an invalid type.

inline LISP& value(LISP x)         // Symbol value
{
  return ((CSYM *) x)->value;
}

inline LISP& prop(LISP x)          // Symbol property list
{
  return ((CSYM *) x)->prop;
}

inline LISP& hash(LISP x)          // Symbol hash linkage
{
  return ((CSYM *) x)->hash;
}

inline char* name(LISP x)          // Symbol print name
{
  return ((CSYM *) x)->name;
}

inline char& chr(LISP x)           // Character value
{
  return ((CCHR *) x)->chr;
}

inline char* text(LISP x)          // String value
{
  return ((CSTR *) x)->text;
}

inline double& rvalue(LISP x)      // Real numeric value
{
  return ((CFLT *) x)->rvalue;
}

inline int& ivalue(LISP x)         // Integer numeric value
{
  return ((CINT *) x)->ivalue;
}

inline int& dim(LISP x)            // Vector dimension
{
  return ((CVECT *) x)->dim;
}

inline LISP* data(LISP x)          // Vector data
{
  return ((CVECT *) x)->data;
}

inline pointer& code(LISP x)       // Primitive function pointer
{
  return ((CEXEC *) x)->code;
}

inline FILE*& file(LISP x)         // File object handle
{
  return ((CFILE *) x)->file;
}

inline int& window(LISP x)         // Window  object handle
{
  return ((CWIND *) x)->window;
}

inline int& width(LISP x)          // Window width
{
  return ((CWIND *) x)->width;
}

inline int& height(LISP x)         // Window height
{
  return ((CWIND *) x)->height;
}

inline View2& view2(LISP x)        // Window 2D view transform
{
  return ((CWIND *) x)->view2;
}

inline LISP& reloc(LISP x)         // Forwarding address pointer (GC)
{
  return ((CRELOC *) x)->reloc;
}

// Object type checking functions
inline bool IsCons(LISP c)
{
  return c && (c->flags == tCONS);
}

inline bool IsList(LISP c)
{
  return !c || (c->flags == tCONS);
}

inline bool IsAtom(LISP c)
{
  return !c || ((c->flags != tCONS) && (c->flags != tVECT));
}

inline bool IsSymbol(LISP c)
{
  return (c && ((c->flags == tSYM)   ||
                (c->flags == tCONST) ||
                (c->flags == tKEY)));
}

inline bool IsVariable(LISP c)
{
  return (c && (c->flags == tSYM));
}

inline bool IsChar(LISP c)
{
  return (c && (c->flags == tCHR));
}

inline bool IsConstant(LISP c)
{
  return (c && ((c->flags == tCONST) || (c->flags == tKEY)));
}

inline bool IsKey(LISP c)
{
  return (c && (c->flags == tKEY));
}

inline bool IsString(LISP c)
{
  return (c && (c->flags == tSTR));
}

inline bool IsInteger(LISP c)
{
  return c && (c->flags == tINT);
}

inline bool IsReal(LISP c)
{
  return c && (c->flags == tREAL);
}

inline bool IsNumber(LISP c)
{
  return c && ((c->flags == tINT) || (c->flags == tREAL));
}

inline bool IsVector(LISP c)
{
  return c && ((c->flags == tVECT) || (c->flags == tCVECT));
}

inline bool IsCVector(LISP c)
{
  return c && (c->flags == tCVECT);
}

inline bool IsSubr(LISP c)
{
  return c && (c->flags == tSUBR);
}

inline bool IsFSubr(LISP c)
{
  return c && (c->flags == tFSUBR);
}

inline bool IsSubrl(LISP c)
{
  return c && (c->flags == tSUBRL);
}

inline bool IsFSubrl(LISP c)
{
  return c && (c->flags == tFSUBRL);
}

inline bool IsExec(LISP c)
{
  return c && ((c->flags == tSUBR)  || (c->flags == tFSUBR) ||
               (c->flags == tSUBRL) || (c->flags == tFSUBRL)  );
}

inline bool IsFile(LISP c)
{
  return c && (c->flags == tFILE);
}

inline bool IsWindow(LISP c)
{
  return c && (c->flags == tWIND);
}

inline bool IsMoved(LISP c)        // Moved (forwarded) cell (GC)
{
  return (c->flags == tRELOC);
}

// Convert c++ bool to Lisp bool
inline LISP Boolean(bool b)
{
  return b ? T : NIL;
}

//HEAP //////////////////////////////////////////////////////////////////////
// Globals relating to the heap
extern pointer heap;               // Active heap
extern integer hsize;              // Heap size
extern pointer hfree;              // Heap free pointer
extern pointer hlimit;             // Heap end + 1
extern pointer gcheap;             // Inactive heap
extern pointer gcfree;             // Inactive heap free pointer
extern LISP hashtable[HASHSIZE];

// Prototypes for externally called heap functions
extern void Heap(integer size);
extern void HeapInit();
extern void RelinkHeapGlobals();
extern void FreeHeap();
extern LISP HeapAlloc(integer size);
extern LISP NewCons(LISP car, LISP cdr);
extern LISP NewSym(char *str, bool intern = true);
extern LISP NewConstant(char *str);
extern LISP NewConstant(char *str, LISP init);
extern LISP NewKey(char *str);
extern LISP NewChar(char val);
extern LISP NewString(int len);
extern LISP NewString(const char *s);
extern LISP NewInteger(int val);
extern LISP NewReal(double val);
extern LISP NewVector(int dimension, LISP init);
extern LISP NewFile(FILE* f);
extern LISP NewSubr(SUBR code);
extern LISP NewFSubr(SUBR code);
extern LISP NewSubrl(SUBRL code);
extern LISP NewFSubrl(FSUBRL code);
extern LISP NewWindow(int win, int w, int h);
extern LISP FindSym(char *name);
extern LISP Set(LISP c, LISP value);
extern void GC();

// Internal heap functions
void MoveCell(LISP& cell);
void Sweep();
void FixHash();
int  Hash(char *s);
bool InHeap(LISP l);

// Debug functions
extern int  DumpHeap(bool v);
bool        HeapCheck(LISP l);
extern void DumpHashtable(void);

// Stack operations on lists
inline LISP Pop(LISP& s)
{
  LISP tos = car(s);
  s = cdr(s);
  return tos;
}

inline void Push(LISP tos, LISP& s)
{
  s = NewCons(tos, s);
}

//STACK /////////////////////////////////////////////////////////////////////
// Frames may be save frames or arg frames
// Arg frames contain the evaluated list of args to a (F)SUBR
// Save frames contain the saved environment for each arg to a (F)EXPR
// The saved data looks like this (arg1 being the first formal arg etc.)
//    LISP arg1
//    LISP arg1->value;
//    LISP arg2
//    LISP arg2->value;
//    ...
struct Frame
{
  word last;            // Negated offset to last frame (always +ve)
  byte nargs;           // Number of args in frame
  bool save;            // Type (Save frame or Arg frame)
  LISP fun;             // Pointer to function
  LISP args[];          // Args stored from here
};

typedef Frame *PFrame;

// Globals relating to the stack
extern pointer stack;   // Base of allocated stack memory
extern pointer slimit;  // End of allocated stack memory
extern pointer stop;    // Next free entry
extern PFrame  base;    // Base of current frame

// Prototypes for stack related functions
extern void Stack(integer size);
extern void FreeStack();
extern void StackReset();
extern void NewFrame(LISP fun);
extern void PopFrame();
extern void PushF(LISP c);
extern LISP PopF();
extern void Local(LISP sym, LISP init = UNDEFINED);
extern void EvArgs(LISP fargs);
extern LISP EvList(LISP fargs);
extern void PushArgs(LISP fargs);
extern void Bind(LISP formals);
extern void SweepStack();
extern void ClearStack(bool enable_bt);
extern void BacktraceDump(int n = 0);

//BALL //////////////////////////////////////////////////////////////////////
// Used by lisp's catch and throw (obviously...)
struct Ball
{
  LISP tag;       // Catch tag
  LISP value;     // Payload
  Ball(LISP tag, LISP value) : tag(tag), value(value) {};
};

//---------------------------------------------------------------------------
#endif
