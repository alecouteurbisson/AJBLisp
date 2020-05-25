////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// store.cc
//
// Heap and stack management including garbage collection
////////////////////////////////////////////////////////////////////////////
#include "ajblisp.h"

// Globals relating to the heap
pointer heap;                   // Base of current heap
integer hsize;                  // Size...
pointer hfree;                  // Free pointer
pointer hlimit;                 // End of free space
pointer gcheap;                 // The other heap
pointer gcfree;                 // and its free pointer

LISP hashtable[HASHSIZE];       // The lisp namespace hashtable

// Globals relating to the stack
pointer stack;                  // Base of allocated stack memory
pointer slimit;                 // End of allocated stack memory
pointer stop;                   // Next free entry
PFrame  base;                   // Base of current frame

// Protected pointer list
pp* pp::pphead = 0;

// Backtrace frame limit
int backtrace = 0;

// GC Counter
int GCCount = 0;

// Cons node counter
int ConsCount = 0;

// Character atom cache
CCHR  CharCache[256];

// Small int cache
CINT  IntCache[256];

//---------------------------------------------------------------------------
// Cells referenced from the interpreter
LISP UNDEFINED;                 // Initial value of symbols
LISP T;                         // Truth
LISP LAMBDA;                    // Function marker
LISP MACRO;                     // Macro marker
LISP DOT;                       // The CONS operator
LISP EOF_;                      // End of file
LISP QUOTE;                     // Quote function
LISP SYSTEM;                    // System command processor

// File open modes
LISP READ;
LISP WRITE;
LISP CREATE;

// String compare modes
LISP CASE;
LISP NOCASE;
LISP COLLATE;

// Seek origin modes
LISP SK_START;
LISP SK_END;
LISP SK_REL;

// Object types
LISP lCONS;
LISP lSYMBOL;
LISP lCONSTANT;
LISP lKEY;
LISP lCHARACTER;
LISP lSTRING;
LISP lINTEGER;
LISP lFLOAT;
LISP lVECTOR;
LISP lCVECTOR;
LISP lSUBR;
LISP lSUBRL;
LISP lFSUBR;
LISP lFSUBRL;
LISP lFILE;
LISP lWINDOW;

// Stream constants
LISP STDIN;
LISP STDOUT;
LISP STDERR;

// Error object flag
LISP NONE;

// Case default
LISP ELSE;

// History vars
LISP PERCENT;
LISP PERCENT1;
LISP PERCENT2;
LISP PERCENT3;
LISP PERCENT4;
LISP PERCENT5;
LISP PERCENT6;

//---------------------------------------------------------------------------
// Heap functions

// Create the heaps and enter all system defined data
void Heap(integer size)
{
  gcheap = new byte[size];
  if(!gcheap)
    LispError(FATAL_HEAP);

  heap = new byte[size];
  if(!heap)
    LispError(FATAL_HEAP);

  hsize = size;
  hlimit = heap + size;
  hfree = heap;
  HeapInit();

  // Initialise character and small integer caches
  // (not stored in lisp heap)
  for(int i = 0; i < 256; i++)
  {
    CharCache[i].flags = tCHR;
    CharCache[i].size = CHR_SIZE;
    CharCache[i].chr = (char)i;

    IntCache[i].flags = tINT;
    IntCache[i].size = INT_SIZE;
    IntCache[i].ivalue = i-128;
  }
}

// Delete both heaps
void FreeHeap()
{
  if(heap)
    delete[]heap;

  if(gcheap)
    delete[]gcheap;
}

void HeapInit()
{
  // Define constant error codes
  DefineErrorConst();

  // Initialise constants and keys
  // Those asssigned to C++ variables are referenced
  // from within the interpreter
  NewConstant(":pi",  NewReal(M_PI));
  NewConstant(":e",   NewReal(M_E));

  NewConstant("%");
  NewConstant("%1");
  NewConstant("%2");
  NewConstant("%3");
  NewConstant("%4");
  NewConstant("%5");
  NewConstant("%6");

  // File open keys
  NewKey(":read");
  NewKey(":write");
  NewKey(":create");

  // String compare keys
  NewKey(":case");
  NewKey(":nocase");
  NewKey(":collate");

  // Seek origin keys
  NewKey(":seek-start");
  NewKey(":seek-end");
  NewKey(":seek-relative");

  // Type constants
  NewKey(":cons");
  NewKey(":symbol");
  NewKey(":constant");
  NewKey(":key");
  NewKey(":character");
  NewKey(":string");
  NewKey(":integer");
  NewKey(":float");
  NewKey(":vector");
  NewKey(":cvector");
  NewKey(":subr");
  NewKey(":subrl");
  NewKey(":fsubr");
  NewKey(":fsubrl");
  NewKey(":file");
  NewKey(":window");

  // No object key
  // This is defined because all other objects, especially nil and :undefined
  // might be usefully used as the error object.  This key exists solely to
  // represent 'no object'
  NewKey(":none");

  // The default case key
  NewKey(":else");

  // Stream constants
  NewConstant(":stdin",         NewFile(stdin));
  NewConstant(":stdout",        NewFile(stdout));
  NewConstant(":stderr",        NewFile(stderr));

  // Useful for printing
  NewConstant(":space",         NewChar(' '));
  NewConstant(":nl",            NewChar('\n'));
  NewConstant(":cr",            NewChar('\r'));
  NewConstant(":tab",           NewChar('\t'));
  NewConstant(":lpar",          NewChar('('));
  NewConstant(":rpar",          NewChar(')'));


  // Graphic function constants
  // Pen styles
  NewConstant(":ps_solid",      NewInteger(0));
  NewConstant(":ps_dash",       NewInteger(-1));
  NewConstant(":ps_dot",        NewInteger(-2));
  NewConstant(":ps_dashdot",    NewInteger(-3));
  NewConstant(":ps_dashdotdot", NewInteger(-4));
  NewConstant(":ps_clear",      NewInteger(-5));

  // Brush styles
  NewConstant(":bs_solid",      NewInteger(0));
  NewConstant(":bs_clear",      NewInteger(1));
  NewConstant(":bs_horiz",      NewInteger(2));
  NewConstant(":bs_vert",       NewInteger(3));
  NewConstant(":bs_fdiag",      NewInteger(4));
  NewConstant(":bs_bdiag",      NewInteger(5));
  NewConstant(":bs_square",     NewInteger(6));
  NewConstant(":bs_diamond",    NewInteger(7));

  // Font styles
  NewConstant(":fs_bold",       NewInteger(1));
  NewConstant(":fs_italic",     NewInteger(2));
  NewConstant(":fs_underline",  NewInteger(4));
  NewConstant(":fs_strikeout",  NewInteger(8));

  // Font pitch
  NewConstant(":fp_variable",   NewInteger(1));
  NewConstant(":fp_fixed",      NewInteger(2));

  // Primitive functions
  iSet(NewSym("quote"),                   NewFSubr(Quote));
  iSet(NewSym("symbol-value"),            NewSubr(SymbolValue));
  iSet(NewSym("eq"),                      NewSubr(Eq));
  iSet(NewSym("eql"),                     NewSubr(Eql));
  iSet(NewSym("equal"),                   NewSubr(Equal));
  iSet(NewSym("cons"),                    NewSubr(Cons));
  iSet(NewSym("not"),                     NewSubr(Not));
  iSet(NewSym("null"),                    NewSubr(Not));
  iSet(NewSym("and"),                     NewFSubrl(And));
  iSet(NewSym("or"),                      NewFSubrl(Or));
  iSet(NewSym("xor"),                     NewFSubrl(Xor));
  iSet(NewSym("bnot"),                    NewSubr(Bnot));
  iSet(NewSym("band"),                    NewSubrl(Band));
  iSet(NewSym("bor"),                     NewSubrl(Bor));
  iSet(NewSym("bxor"),                    NewSubrl(Bxor));
  iSet(NewSym("bset"),                    NewSubr(Bset));
  iSet(NewSym("bclr"),                    NewSubr(Bclr));
  iSet(NewSym("btgl"),                    NewSubr(Btgl));
  iSet(NewSym("btst"),                    NewSubr(Btst));
  iSet(NewSym("bcnt"),                    NewSubr(Bcnt));
  iSet(NewSym("bshift"),                  NewSubr(Bshift));
  iSet(NewSym("ashift"),                  NewSubr(Ashift));
  iSet(NewSym("setq"),                    NewFSubrl(Setq));
  iSet(NewSym("psetq"),                   NewFSubrl(Psetq));
  iSet(NewSym("set"),                     NewSubr(Set));
  iSet(NewSym("car"),                     NewSubr(Car));
  iSet(NewSym("cdr"),                     NewSubr(Cdr));
  iSet(NewSym("caar"),                    NewSubr(Caar));
  iSet(NewSym("cadr"),                    NewSubr(Cadr));
  iSet(NewSym("cdar"),                    NewSubr(Cdar));
  iSet(NewSym("cddr"),                    NewSubr(Cddr));
  iSet(NewSym("caaar"),                   NewSubr(Caaar));
  iSet(NewSym("caadr"),                   NewSubr(Caadr));
  iSet(NewSym("cadar"),                   NewSubr(Cadar));
  iSet(NewSym("caddr"),                   NewSubr(Caddr));
  iSet(NewSym("cdaar"),                   NewSubr(Cdaar));
  iSet(NewSym("cdadr"),                   NewSubr(Cdadr));
  iSet(NewSym("cddar"),                   NewSubr(Cddar));
  iSet(NewSym("cdddr"),                   NewSubr(Cdddr));
  iSet(NewSym("rplaca"),                  NewSubr(Rplaca));
  iSet(NewSym("rplacd"),                  NewSubr(Rplacd));
  iSet(NewSym("eval"),                    NewSubr(Eval));
  iSet(NewSym("apply"),                   NewSubr(Apply));
  iSet(NewSym("length"),                  NewSubr(Length));
  iSet(NewSym("+"),                       NewSubrl(Add));
  iSet(NewSym("-"),                       NewSubr(Subtract));
  iSet(NewSym("*"),                       NewSubrl(Multiply));
  iSet(NewSym("/"),                       NewSubr(Divide));
  iSet(NewSym("rem"),                     NewSubr(Rem));
  iSet(NewSym("inc"),                     NewFSubr(Inc));
  iSet(NewSym("dec"),                     NewFSubr(Dec));
  iSet(NewSym(">"),                       NewSubr(Greaterp));
  iSet(NewSym("<"),                       NewSubr(Lessp));
  iSet(NewSym("<="),                      NewSubr(NGreaterp));
  iSet(NewSym(">="),                      NewSubr(NLessp));
  iSet(NewSym("="),                       NewSubr(Equalp));
  iSet(NewSym("<>"),                      NewSubr(NEqualp));
  iSet(NewSym("zerop"),                   NewSubr(Zerop));
  iSet(NewSym("onep"),                    NewSubr(Onep));
  iSet(NewSym("plusp"),                   NewSubr(Plusp));
  iSet(NewSym("minusp"),                  NewSubr(Minusp));
  iSet(NewSym("atom"),                    NewSubr(Atom));
  iSet(NewSym("symbolp"),                 NewSubr(Symbolp));
  iSet(NewSym("constantp"),               NewSubr(Constantp));
  iSet(NewSym("keyp"),                    NewSubr(Keyp));
  iSet(NewSym("variablep"),               NewSubr(Variablep));
  iSet(NewSym("numberp"),                 NewSubr(Numberp));
  iSet(NewSym("integerp"),                NewSubr(Integerp));
  iSet(NewSym("floatp"),                  NewSubr(Floatp));
  iSet(NewSym("listp"),                   NewSubr(Listp));
  iSet(NewSym("consp"),                   NewSubr(Consp));
  iSet(NewSym("charp"),                   NewSubr(Charp));
  iSet(NewSym("stringp"),                 NewSubr(Stringp));
  iSet(NewSym("execp"),                   NewSubr(Execp));
  iSet(NewSym("subrp"),                   NewSubr(Subrp));
  iSet(NewSym("fsubrp"),                  NewSubr(FSubrp));
  iSet(NewSym("vectorp"),                 NewSubr(Vectorp));
  iSet(NewSym("filep"),                   NewSubr(Filep));
  iSet(NewSym("windowp"),                 NewSubr(Windowp));
  iSet(NewSym("freep"),                   NewSubr(Freep));
  iSet(NewSym("typep"),                   NewSubr(Typep));
  iSet(NewSym("lexcmp"),                  NewSubr(LexCmp));
  iSet(NewSym("if"),                      NewFSubr(If));
  iSet(NewSym("cond"),                    NewFSubrl(Cond));
  iSet(NewSym("for"),                     NewFSubrl(For));
  iSet(NewSym("for-each"),                NewFSubrl(ForEach));
  iSet(NewSym("loop"),                    NewFSubrl(Loop));
  iSet(NewSym("until"),                   NewFSubrl(Until));
  iSet(NewSym("while"),                   NewFSubrl(While));
  iSet(NewSym("break"),                   NewFSubrl(Break));
  iSet(NewSym("progn"),                   NewFSubrl(Progn));
  iSet(NewSym("prog1"),                   NewFSubrl(Prog1));
  iSet(NewSym("when"),                    NewFSubrl(When));
  iSet(NewSym("unless"),                  NewFSubrl(Unless));
  iSet(NewSym("case"),                    NewFSubrl(Case));
  iSet(NewSym("let"),                     NewFSubrl(Let));
  iSet(NewSym("slet"),                    NewFSubrl(Slet));
  iSet(NewSym("list"),                    NewSubrl(List));
  iSet(NewSym("member"),                  NewSubr(Member));
  iSet(NewSym("append"),                  NewSubrl(Append));
  iSet(NewSym("delete"),                  NewSubr(Delete));
  iSet(NewSym("filter"),                  NewSubr(Filter));
  iSet(NewSym("mapc"),                    NewSubr(Mapc));
  iSet(NewSym("reverse"),                 NewSubr(Reverse));
  iSet(NewSym("nth"),                     NewSubr(Nth));
  iSet(NewSym("nthcdr"),                  NewSubr(NthCdr));
  iSet(NewSym("last"),                    NewSubr(Last));
  iSet(NewSym("push"),                    NewFSubr(Push));
  iSet(NewSym("pop"),                     NewFSubr(Pop));
  iSet(NewSym("open-file"),               NewSubr(OpenFile));
  iSet(NewSym("close-file"),              NewSubr(CloseFile));
  iSet(NewSym("file-position"),           NewSubr(FilePosition));
  iSet(NewSym("float-format"),            NewSubr(FloatFormat));
  iSet(NewSym("integer-format"),          NewSubr(IntegerFormat));
  iSet(NewSym("logfile"),                 NewSubr(LogFile));
  iSet(NewSym("write"),                   NewSubrl(Write));
  iSet(NewSym("print"),                   NewSubrl(Print));
  iSet(NewSym("writeln"),                 NewSubrl(Writeln));
  iSet(NewSym("println"),                 NewSubrl(Println));
  iSet(NewSym("print-format"),            NewSubrl(PrintFormat));
  iSet(NewSym("write-size"),              NewSubrl(WriteSize));
  iSet(NewSym("print-size"),              NewSubrl(PrintSize));
  iSet(NewSym("read"),                    NewSubr(Read));
  iSet(NewSym("readline"),                NewSubr(Readline));
  iSet(NewSym("readchar"),                NewSubr(Readchar));
  iSet(NewSym("writechar"),               NewSubr(Writechar));
  iSet(NewSym("readbyte"),                NewSubr(Readbyte));
  iSet(NewSym("writebyte"),               NewSubr(Writebyte));
  iSet(NewSym("readword"),                NewSubr(Readword));
  iSet(NewSym("writeword"),               NewSubr(Writeword));
  iSet(NewSym("readlong"),                NewSubr(Readlong));
  iSet(NewSym("writelong"),               NewSubr(Writelong));
  iSet(NewSym("unread"),                  NewSubr(Unread));
  iSet(NewSym("eof"),                     NewSubr(Eof));
  iSet(NewSym("flush"),                   NewSubr(Flush));
  iSet(NewSym("abs"),                     NewSubr(Abs));
  iSet(NewSym("sign"),                    NewSubr(Sign));
  iSet(NewSym("float"),                   NewSubr(Float));
  iSet(NewSym("integer"),                 NewSubr(Integer));
  iSet(NewSym("floor"),                   NewSubr(Floor));
  iSet(NewSym("ceil"),                    NewSubr(Ceil));
  iSet(NewSym("sqrt"),                    NewSubr(Sqrt));
  iSet(NewSym("sin"),                     NewSubr(Sin));
  iSet(NewSym("cos"),                     NewSubr(Cos));
  iSet(NewSym("tan"),                     NewSubr(Tan));
  iSet(NewSym("sinh"),                    NewSubr(Sinh));
  iSet(NewSym("cosh"),                    NewSubr(Cosh));
  iSet(NewSym("tanh"),                    NewSubr(Tanh));
  iSet(NewSym("asin"),                    NewSubr(Asin));
  iSet(NewSym("acos"),                    NewSubr(Acos));
  iSet(NewSym("atan"),                    NewSubr(Atan));
  iSet(NewSym("asinh"),                   NewSubr(Asinh));
  iSet(NewSym("acosh"),                   NewSubr(Acosh));
  iSet(NewSym("atanh"),                   NewSubr(Atanh));
  iSet(NewSym("log"),                     NewSubr(Log));
  iSet(NewSym("ln"),                      NewSubr(Ln));
  iSet(NewSym("exp"),                     NewSubr(Exp));
  iSet(NewSym("power"),                   NewSubr(Power));
  iSet(NewSym("random"),                  NewSubr(Random));
  iSet(NewSym("assoc"),                   NewSubr(Assoc));
  iSet(NewSym("cdrassoc"),                NewSubr(CdrAssoc));
  iSet(NewSym("put"),                     NewSubr(Put));
  iSet(NewSym("get"),                     NewSubr(Get));
  iSet(NewSym("remprop"),                 NewSubr(Remprop));
  iSet(NewSym("plist"),                   NewSubr(Plist));
  iSet(NewSym("defconstant"),             NewFSubrl(DefConstant));
  iSet(NewSym("defkey"),                  NewFSubrl(DefKey));
  iSet(NewSym("destroy"),                 NewSubrl(Destroy));
  iSet(NewSym("oblist"),                  NewSubr(Oblist));
  iSet(NewSym("concat"),                  NewSubrl(Concat));
  iSet(NewSym("substring"),               NewSubr(Substring));
  iSet(NewSym("symbol"),                  NewSubr(Symbol));
  iSet(NewSym("free"),                    NewSubr(Free));
  iSet(NewSym("string"),                  NewSubr(String));
  iSet(NewSym("chars"),                   NewSubr(Chars));
  iSet(NewSym("char"),                    NewSubr(Char));
  iSet(NewSym("schar"),                   NewSubr(Schar));
  iSet(NewSym("upper-casep"),             NewSubr(UpperCasep));
  iSet(NewSym("lower-casep"),             NewSubr(LowerCasep));
  iSet(NewSym("char="),                   NewSubr(CharEqual));
  iSet(NewSym("char<>"),                  NewSubr(CharNEqual));
  iSet(NewSym("char>"),                   NewSubr(CharGreater));
  iSet(NewSym("char<"),                   NewSubr(CharLess));
  iSet(NewSym("char>="),                  NewSubr(CharNLess));
  iSet(NewSym("char<="),                  NewSubr(CharNGreater));
  iSet(NewSym("string="),                 NewSubr(StringEqual));
  iSet(NewSym("string<>"),                NewSubr(StringNEqual));
  iSet(NewSym("string>"),                 NewSubr(StringGreater));
  iSet(NewSym("string<"),                 NewSubr(StringLess));
  iSet(NewSym("string>="),                NewSubr(StringNLess));
  iSet(NewSym("string<="),                NewSubr(StringNGreater));
  iSet(NewSym("upper-case"),              NewSubr(UpperCase));
  iSet(NewSym("lower-case"),              NewSubr(LowerCase));
  iSet(NewSym("string-search"),           NewSubr(StringSearch));
  iSet(NewSym("gensym"),                  NewSubr(Gensym));
  iSet(NewSym("tokenise"),                NewSubr(Tokenise));
  iSet(NewSym("make-vector"),             NewSubr(MakeVector));
  iSet(NewSym("vlist"),                   NewSubr(Vlist));
  iSet(NewSym("vector"),                  NewSubr(Vector));
  iSet(NewSym("vref"),                    NewSubr(Vref));
  iSet(NewSym("vset"),                    NewSubr(Vset));
  iSet(NewSym("vsort"),                   NewSubr(Vsort));
  iSet(NewSym("locale"),                  NewSubr(Locale));
  iSet(NewSym("tzset"),                   NewSubr(Tzset));
  iSet(NewSym("local-time"),              NewSubr(LocalTime));
  iSet(NewSym("dst"),                     NewSubr(Dst));
  iSet(NewSym("now"),                     NewFSubrl(Now));
  iSet(NewSym("date"),                    NewSubr(Date));
  iSet(NewSym("time"),                    NewSubr(Time));
  iSet(NewSym("encode-time"),             NewSubr(EncodeTime));
  iSet(NewSym("date-time"),               NewSubr(DateTime));
  iSet(NewSym("time-string"),             NewSubr(TimeString));
  iSet(NewSym("timer"),                   NewFSubrl(Timer));
  iSet(NewSym("load"),                    NewSubr(Load));
  iSet(NewSym("typeof"),                  NewSubr(Typeof));
  iSet(NewSym("collect"),                 NewSubr(Collect));
  iSet(NewSym("error"),                   NewSubr(Error));
  iSet(NewSym("errorset"),                NewFSubrl(Errorset));
  iSet(NewSym("errormsg"),                NewSubr(ErrorMsg));
  iSet(NewSym("guard"),                   NewFSubrl(Guard));
  iSet(NewSym("catch"),                   NewFSubrl(Catch));
  iSet(NewSym("throw"),                   NewSubr(Throw));
  iSet(NewSym("backtrace"),               NewSubr(Backtrace));
  iSet(NewSym("exit"),                    NewFSubrl(Exit));
  iSet(NewSym("delete-file"),             NewSubr(lDeleteFile));
  iSet(NewSym("move-file"),               NewSubr(lMoveFile));
  iSet(NewSym("copy-file"),               NewSubr(lCopyFile));
  iSet(NewSym("protect-file"),            NewSubr(ProtectFile));
  iSet(NewSym("create-directory"),        NewSubr(lCreateDirectory));
  iSet(NewSym("delete-directory"),        NewSubr(lDeleteDirectory));
  iSet(NewSym("dir"),                     NewSubr(Dir));
  iSet(NewSym("cd"),                      NewSubr(Cd));
  iSet(NewSym("system"),                  NewSubr(lSystem));
  iSet(NewSym("sleep"),                   NewSubr(lSleep));
  iSet(NewSym("scripted"),                NewFSubrl(Scripted));
  iSet(NewSym("environ"),                 NewSubr(Environ));
  iSet(NewSym("status"),                  NewSubr(Status));
  iSet(NewSym("debug"),                   NewSubr(Debug));
  iSet(NewSym("addr"),                    NewSubr(Addr));
  iSet(NewSym("dump"),                    NewFSubrl(Dump));
  iSet(NewSym("dumphash"),                NewFSubrl(DumpHash));
  iSet(NewSym("statistics"),              NewFSubrl(Statistics));

  // Graphics primitives
  iSet(NewSym("gwindow"),                 NewSubr(GWindow));
  iSet(NewSym("gclose"),                  NewFSubr(GClose));
  iSet(NewSym("gclear"),                  NewSubr(GClear));
  iSet(NewSym("gmove"),                   NewSubr(GMove));
  iSet(NewSym("gpos"),                    NewSubr(GPos));
  iSet(NewSym("gdraw"),                   NewSubr(GDraw));
  iSet(NewSym("gpoint"),                  NewSubr(GPoint));
  iSet(NewSym("gpick"),                   NewSubr(GPick));
  iSet(NewSym("gellipse"),                NewSubr(GEllipse));
  iSet(NewSym("grectangle"),              NewSubr(GRectangle));
  iSet(NewSym("gtext"),                   NewSubr(GText));
  iSet(NewSym("gtextsize"),               NewSubr(GTextSize));
  iSet(NewSym("gviewport"),               NewSubr(GViewport));
  iSet(NewSym("gvtrans2"),                NewSubr(GVTrans2));
  iSet(NewSym("gpen"),                    NewSubr(GPen));
  iSet(NewSym("gbrush"),                  NewSubr(GBrush));
  iSet(NewSym("gcolour"),                 NewSubr(GColour));
  iSet(NewSym("gfont"),                   NewSubr(GFont));

  // Some important system constants
  NewConstant(":undefined");
  NewConstant("t");
  NewConstant("lambda");
  NewConstant("macro");
  NewConstant(".");
  NewConstant(":eof");

  RelinkHeapGlobals();
}

void RelinkHeapGlobals()
{
  // File open keys
  READ       = FindSym(":read");
  WRITE      = FindSym(":write");
  CREATE     = FindSym(":create");

  // String compare keys
  CASE       = FindSym(":case");
  NOCASE     = FindSym(":nocase");
  COLLATE    = FindSym(":collate");

  // Seek origin keys
  SK_START   = FindSym(":seek-start");
  SK_END     = FindSym(":seek-end");
  SK_REL     = FindSym(":seek-relative");

  // Type constants
  lCONS      = FindSym(":cons");
  lSYMBOL    = FindSym(":symbol");
  lCONSTANT  = FindSym(":constant");
  lKEY       = FindSym(":key");
  lCHARACTER = FindSym(":character");
  lSTRING    = FindSym(":string");
  lINTEGER   = FindSym(":integer");
  lFLOAT     = FindSym(":float");
  lVECTOR    = FindSym(":vector");
  lCVECTOR   = FindSym(":cvector");
  lSUBR      = FindSym(":subr");
  lSUBRL     = FindSym(":subrl");
  lFSUBR     = FindSym(":fsubr");
  lFSUBRL    = FindSym(":fsubrl");
  lFILE      = FindSym(":file");
  lWINDOW    = FindSym(":window");

  // No object key
  NONE       = FindSym(":none");

  // The default case key
  ELSE       = FindSym(":else");

  // Functions
  QUOTE      = FindSym("quote");
  SYSTEM     = FindSym("system");

  // Some important system constants
  UNDEFINED  = FindSym(":undefined");
  T          = FindSym("t");
  LAMBDA     = FindSym("lambda");
  MACRO      = FindSym("macro");
  DOT        = FindSym(".");
  EOF_       = FindSym(":eof");

  // Result history
  PERCENT    = FindSym("%");
  PERCENT1   = FindSym("%1");
  PERCENT2   = FindSym("%2");
  PERCENT3   = FindSym("%3");
  PERCENT4   = FindSym("%4");
  PERCENT5   = FindSym("%5");
  PERCENT6   = FindSym("%6");
}

// Grab a chunk of the heap
LISP HeapAlloc(integer size)
{
  pointer c = hfree;

  hfree += size;
  if(hfree >= hlimit)
  {
    GC();
    c = hfree;
    hfree += size;
    if(hfree > hlimit)
      LispError(FATAL_HEAP_OVF);
  }
  // Keep word aligned
  if((int)hfree & 1) hfree++;
  return LISP(c);
}

// Lisp object constructors

// Create an initialised cons cell
LISP NewCons(LISP a, LISP d)
{
  ConsCount++;
  protect(pcar, a);
  protect(pcdr, d);
  LISP c = HeapAlloc(CONS_SIZE);

  flags(c) = tCONS;
  size(c) = CONS_SIZE;
  cdr(c) = pcdr;
  car(c) = pcar;
  return c;
}

// Create a symbol with a given printname
LISP NewSym(char *str, bool intern /* = true */)
{
#ifdef UPPER_CASE_SYMBOLS
  strupr(str);  // For the hard of hearing ;)
#endif

  int len = strlen(str);
  if(len > (256 - SYM_SIZE))
    LispError(ERR_SYM_LEN);

  int alloc = len + SYM_SIZE;
  LISP c = HeapAlloc(alloc);

  flags(c) = tSYM;
  size(c) = (byte) (len + SYM_SIZE);
  value(c) = UNDEFINED;
  prop(c) = NULL;
  memcpy(name(c), str, len);

  if(intern)
  {
    // Link symbol into hashchain
    int hashidx = Hash(str);

    hash(c) = hashtable[hashidx];
    hashtable[hashidx] = c;
  }
  else
  {
    hash(c) = 0L;
  }
  return c;
}

// Create a constant symbol with the given name
// preset to be equal to itself
LISP NewConstant(char *str)
{
  LISP c = NewSym(str);
  flags(c) = tCONST;
  value(c) = c;
  return c;
}

// Create a constant symbol with the given printname and value
LISP NewConstant(char *str, LISP init)
{
  LISP c = NewConstant(str);
  value(c) = init;
  return c;
}

// Create a key
LISP NewKey(char *str)
{
  LISP c = NewSym(str);
  flags(c) = tKEY;
  return c;
}

// Create a character atom
LISP NewChar(char val)
{
  // Just supply the one from the cache
  return &CharCache[val];
}

// Create a string
// This version just allocates storage
LISP NewString(int len)
{
  LISP c;
  c = HeapAlloc(STR_SIZE + len);
  flags(c) = tSTR;
  size(c) = 0;
  bigsize(c) = STR_SIZE + len;
  text(c)[0] = '\0';
  return c;
}

// Create a string
// This version creates an initialised string
LISP NewString(const char* s)
{
  LISP c = NewString(strlen(s) + 1);
  strcpy(text(c), s);
  return c;
}

// Create an integer
LISP NewInteger(int val)
{
  // Small integers...
  int idx = val + 128;
  if(idx & ~0xFF)
  {
    LISP c = HeapAlloc(INT_SIZE);

    flags(c) = tINT;
    size(c) = INT_SIZE;
    ivalue(c) = val;
    return c;
  }
  else
    // ...come from the cache
    return &IntCache[idx];
}

// Create a float
LISP NewReal(double val)
{
  LISP c = HeapAlloc(REAL_SIZE);

  flags(c) = tREAL;
  size(c) = REAL_SIZE;
  rvalue(c) = val;
  return c;
}

// Create a vector of the given dimension and initialised so that
// all cells contain init
LISP NewVector(int dimension, LISP init)
{
  LISP c;
  int sz = VECT_SIZE + dimension * sizeof(LISP);
  c = HeapAlloc(sz);
  flags(c) = tVECT;
  size(c) = 0;
  bigsize(c) = sz;
  dim(c) = dimension;
  // Initialise if init is not NIL
  if(init)
    for(int i = 0; i < dimension; i++)
      data(c)[i] = init;
  else // Quickly clear to NIL
    memset((void*)data(c), '\0', dimension * sizeof(LISP));
  return c;
}

// Create a file atom
LISP NewFile(FILE* f)
{
  LISP c = NewInteger((int)f);
  flags(c) = tFILE;
  return c;
}

// Create a code placeholder
LISP NewSubr(SUBR exec)
{
  LISP c = HeapAlloc(EXEC_SIZE);

  flags(c) = tSUBR;
  size(c) = EXEC_SIZE;
  code(c) = pointer(exec);
  return c;
}

// Create a code placeholder
LISP NewFSubr(SUBR exec)
{
  LISP c = HeapAlloc(EXEC_SIZE);

  flags(c) = tFSUBR;
  size(c) = EXEC_SIZE;
  code(c) = pointer(exec);
  return c;
}

// Create a code placeholder
LISP NewSubrl(SUBRL exec)
{
  LISP c = HeapAlloc(EXEC_SIZE);

  flags(c) = tSUBRL;
  size(c) = EXEC_SIZE;
  code(c) = pointer(exec);
  return c;
}

// Create a code placeholder
LISP NewFSubrl(FSUBRL exec)
{
  LISP c = HeapAlloc(EXEC_SIZE);

  flags(c) = tFSUBRL;
  size(c) = EXEC_SIZE;
  code(c) = pointer(exec);
  return c;
}

// Create a graphics window atom
LISP NewWindow(int win, int w, int h)
{
  LISP c = HeapAlloc(WIND_SIZE);
  flags(c) = tWIND;
  size(c) = WIND_SIZE;
  window(c) = win;
  width(c) = w;
  height(c) = h;
  view2(c).zero();
  view2(c).vt[0][0] = 1.0;
  view2(c).vt[1][1] = -1.0;
  view2(c).vt[1][2] = double(h);

  return c;
}


// Return a symbol given its name or NULL if not found
LISP FindSym(char *n)
{
#ifdef UPPER_CASE_SYMBOLS
  strupr(n);
#endif
  unsigned int len = strlen(n);
  LISP c = hashtable[Hash(n)];
  while(c)
  {
    if(((size(c) - SYM_SIZE) == len) &&
       !strncmp(n, name(c), len))
      break;
    // Chase the hash-chain
    c = hash(c);
  }
  return c;
}

//---------------------------------------------------------------------------
// The garbage collector
//
// The garbage collector uses a double-space mark and sweep algorithm.
// Both spaces are permanantly allocated.
//
void GC()
{
  GCCount++;
  // writes("Entering GC\n");

  // Grab this before it moves...
  LISP undefined = UNDEFINED;

  // Reset the GC heap
  gcfree = gcheap;

  // Move these special cells first.
  // They are all directly referenced from within the interpreter
  MoveCell(UNDEFINED);      // ... it moved!
  MoveCell(T);
  MoveCell(LAMBDA);
  MoveCell(MACRO);
  MoveCell(DOT);
  MoveCell(EOF_);
  MoveCell(QUOTE);
  MoveCell(SYSTEM);

  // File open modes
  MoveCell(READ);
  MoveCell(WRITE);
  MoveCell(CREATE);

  // String compare modes
  MoveCell(CASE);
  MoveCell(NOCASE);
  MoveCell(COLLATE);

  // Seek origin modes
  MoveCell(SK_START);
  MoveCell(SK_END);
  MoveCell(SK_REL);

  // Object types
  MoveCell(lCONS);
  MoveCell(lSYMBOL);
  MoveCell(lCONSTANT);
  MoveCell(lKEY);
  MoveCell(lCHARACTER);
  MoveCell(lSTRING);
  MoveCell(lINTEGER);
  MoveCell(lFLOAT);
  MoveCell(lVECTOR);
  MoveCell(lCVECTOR);
  MoveCell(lSUBR);
  MoveCell(lSUBRL);
  MoveCell(lFSUBR);
  MoveCell(lFSUBRL);
  MoveCell(lFILE);
  MoveCell(lWINDOW);

  // Stream constants
  MoveCell(STDIN);
  MoveCell(STDOUT);
  MoveCell(STDERR);

  // Error object flag
  MoveCell(NONE);

  // Default case key
  MoveCell(ELSE);

  // History
  MoveCell(PERCENT);
  MoveCell(PERCENT1);
  MoveCell(PERCENT2);
  MoveCell(PERCENT3);
  MoveCell(PERCENT4);
  MoveCell(PERCENT5);
  MoveCell(PERCENT6);

  // Move all protected cells
  pp *p = pp::pphead;
  while(p)
  {
    MoveCell((LISP)p->p);
    p = (pp*)p->next;
  }

  // Call MoveCell for every namespace object except symbols that are
  // undefined and have no property
  for(int i = 0; i < HASHSIZE; i++)
  {
    LISP next = hashtable[i];

    while(next)
    {
      if(!IsSymbol(next) || (value(next) != undefined) || (prop(next)))
        MoveCell(next);
      next = hash(next);     // Chase the hash chain
    }
  }
  // Move stacked references
  SweepStack();
  // Move referenced cells
  Sweep();
  // Fixup the hash chains
  FixHash();

  // Handy debug
  // writef("\nGC: Old Heap=%d New Heap=%d\n", hfree-heap, gcfree-gcheap);

  // Swap heaps
  hfree = gcfree;
  unsigned char* temp = heap;
  heap = gcheap;
  gcheap = temp;

  // Keep word aligned
  if((int)hfree & 1) hfree++;

  hlimit = heap + hsize;
}

// If the referenced cell is in the Heap then move it to the GCHeap
void MoveCell(LISP& cell)
{
  // Don't move nil, cached characters/integers!
  if(!InHeap(cell))
    return;

  if(IsMoved(cell))
    // Update reference
    cell = reloc(cell);
  else
  {
    long sz = getsize(cell);
    // Copy cell to GCHeap
    memcpy(gcfree, (void*)cell, sz);
    // Leave a forwarding address
    reloc(cell) = (LISP)gcfree;
    flags(cell) = tRELOC;
    cell = (LISP)gcfree;
    // Update gcfree
    gcfree += sz;
    // Keep word alignment (CSYMs can upset this)
    if((integer) gcfree & 1)
      gcfree++;
  }
}

// Sweep the new heap for unmoved references
// A single sweep is enough since referenced cells are moved to higher
// addresses and will be swept after they have been moved
void Sweep()
{
  pointer c = gcheap;

  while(pointer(c) < gcfree)
  {
    long sz = getsize(LISP(c));
    // Move referenced cells
    if(IsCons(LISP(c)))
    {
      MoveCell(car(LISP(c)));
      MoveCell(cdr(LISP(c)));
    }
    else if(IsSymbol(LISP(c)) || IsConstant(LISP(c)))
    {
      MoveCell(value(LISP(c)));
      MoveCell(prop(LISP(c)));
      // Hash chains are fixed up later
    }
    else if(IsKey(LISP(c)))
    {
      MoveCell(value(LISP(c)));
    }
    else if(IsVector(LISP(c)))
    {
      LISP* elements = data(LISP(c));
      int i = dim(LISP(c));
      while(i > 0)
        MoveCell(elements[--i]);
    }
    // Next cell
    c += sz;
    // Keep word alignment (CSYMs can upset this)
    if((integer) c & 1)
      c++;
  }
}

// Rebuild the hash chains excluding dead cells
void FixHash()
{
  for(int i = 0; i < HASHSIZE; i++)
  {
    LISP next = hashtable[i];

    // Remove dead cells at head of hash chain
    while(next && !IsMoved(next))
      next = hash(next);
    // Is this chain now empty?
    if(!next)
    {
      // Yup!
      hashtable[i] = NULL;
      continue;
    }
    // hashend is the last cell of the new hashchain
    LISP hashend = hashtable[i] = reloc(next);

    // Scan next through the old hashchain
    while(next)
    {
      // If a valid cell is found
      LISP link = hash(next);
      if(next && IsMoved(next))
      {
        // Link the relocated cell to the new hashchain
        hashend = hash(hashend) = reloc(next);
      }
      next = link;
    }
    // Terminate the new hashchain
    hash(hashend) = NULL;
  }
}

int Hash(char *s)
{
  // Distribution has been tested - looks fine
  // Use (dumphash) for a detailed analysis
  integer hash = strlen(s);

  while(*s)
    hash = (0x9a5d ^ hash) * integer(*s++);

  hash = hash ^ (hash >> 16);
  return hash % HASHSIZE;
}

//---------------------------------------------------------------------------
// Debugging functions for the interpreter heap
//---------------------------------------------------------------------------

// Check that l is in the current heap
bool InHeap(LISP l)
{
  pointer x = pointer(l);
  if((x >= heap) && (x < hlimit))
    return true;
  else
    return false;
}

// Dump the entire heap and report errors
// This is a debugging function and can be called
// from within Lisp as (dump) or (dump t)
int DumpHeap(bool v)
{
  pointer c = heap;
  int errors = 0;

  while((c < hfree) && !UserBreak)
  {
    writef("%8p: ", c);
    if(v)
    {
      if(IsCons(LISP(c)))
        writef("(%8p . %8p) ", car(LISP(c)), cdr(LISP(c)));
      else if(IsSymbol(LISP(c)))
        writef("(%8p / %8p) ", value(LISP(c)), prop(LISP(c)));
      else
        writes("                      ");
    }
    print(LISP(c), stdout, false);

    if(IsSymbol(LISP(c)))
    {
      writes("  Val: ");
      print(value(LISP(c)), stdout, false);
      writes("  Prop: ");
      print(prop(LISP(c)), stdout, false);

    }

    writes("\n");
    if(IsCons(LISP(c)))
    {

      if(!HeapCheck(car(LISP(c))))
        writes("### Error - Car not in heap\n"), errors++;
      if(!HeapCheck(cdr(LISP(c))))
        writes("### Error - Cdr not in heap\n"), errors++;
    }

    if(IsSymbol(LISP(c)))
    {
      if(!HeapCheck(value(LISP(c))))
        writes("### Error - Value not in heap\n"), errors++;
      if(!HeapCheck(prop(LISP(c))))
        writes("### Error - Property not in heap\n"), errors++;
    }

    // Next cell
    int size = getsize(LISP(c));
    if(!size)
    {
       writes("### Error - Zero length object, aborting dump"), errors++;
       return errors;
    }
    c += size;

    // Ensure even alignment
    if((int) c & 1)
      c++;
  }
  if(UserBreak)
    LispError(ERR_USER, NONE, "Dump interrupted");

  return errors;
}

// Return false if l is not in the heap when it should be
bool HeapCheck(LISP l)
{
  if(!l || InHeap(l) || IsChar(l))
    return true;
  if(IsInteger(l) && (ivalue(l) > -127) && (ivalue(l) < 128))
    return true;
  return false;

}

// Dump the symbol hashtable and calcuate the occupancy distribution
void DumpHashtable(void)
{
  int hist[11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

  writes("Hash statistics\n");
  for(int i = 0; i < HASHSIZE; i++)
  {
    int occ = 0;

    writef("Bucket %3d: ", i);
    if(hashtable[i])
    {
      for(LISP sym = hashtable[i]; sym; sym = hash(sym), occ++)
      {
        patom(sym, stdout, false);
        writes(" ");
      }
    }
    writec('\n');
    hist[(occ > 9) ? 10 : occ]++;
  }
  writes("Occupancy histogram\n");
  for(int i = 0; i < 10; i++)
    writef("%5d: %5d\n", i, hist[i]);
  writef("   >9: %5d\n", hist[10]);
}

//---------------------------------------------------------------------------
// Stack functions
void Stack(integer size)
{
  stack = new byte[size];
  if(!stack)
    LispError(FATAL_STACK);

  slimit = stack + size;
  stop = stack;
  base = PFrame(stack);
}

void FreeStack()
{
  delete [] stack;
}

void StackReset()
{
  stop = stack;
  base = PFrame(stack);
}

// Create a frame for the evaluation of expr
void NewFrame(LISP expr)
{
  if((slimit - stop) < 8)
    LispError(FATAL_STACK_OVF);

  PFrame frame = PFrame(stop);

  frame->last = word(stop - pointer(base));  // Set offset to prior frame
  base = frame;                        // New frame base
  frame->nargs = 0;                    // No args (yet)
  frame->save = false;                 // Arg frame initially
  frame->fun = expr;                   // Pointer to sexpr
  stop += sizeof(Frame);               // Update stack top
}

// Pop a frame and restore the environment
void PopFrame()
{
  // Arg frames can be popped without further consideration
  // but Save frames must be restored
  if(base->save)
  {
    // We must now restore the previous environment
    int argp = 0;                      // Index into array of saved args

    for(int n = 0; n < base->nargs; n++)
    {
      LISP arg = base->args[argp++];   // Arg is the cell on the heap

      // This and the remaining args are evaluated but unbound
      if(!arg)
        break;                         // Unbound argument

      if(!IsSymbol(arg))
        LispError(FATAL_BAD_STACK);

      value(arg) = base->args[argp++]; // Restore the old value
    }
  }
  // Restore any saved locals
  LISP* locals = base->args + base->nargs;
  // Save frames are twice the size
  if(base->save)
    locals += base->nargs;

  while(locals < (LISP*)stop)
  {
    if(*locals)
      value(locals[0]) = locals[1];
    locals += 2;
  }

  if(locals != (LISP*)stop)
    LispError(FATAL_BAD_STACK);

  stop = pointer(base);             // Restore old stack top
  base = PFrame(stop - base->last); // Restore old frame base
}

// Push an arg onto the current frame
// This will protect the stacked pointer from GC
void PushF(LISP c)
{
  if(stop == slimit)
    LispError(FATAL_STACK_OVF);

  *(LISP *)stop = c;
  stop += sizeof(LISP);
}

// Pop arg from the current frame
// Returns the arg popped
LISP PopF()
{
  stop -= sizeof(LISP);
  return *(LISP *)stop;
}

// Create a local variable
void Local(LISP sym, LISP init)
{
  PushF(sym);
  PushF(value(sym));
  iSet(sym, init);
}

//-----------------------------------------------------------------
// Argument handling functions

// Evaluate a list of args onto the stack
void EvArgs(LISP fargs)
{
  protect(pfargs, fargs);

  // If this is an arg frame just evaluate each arg and push it
  // If this is a save frame create saved arg entries
  // which are NULL but for the saved value which is the
  // evaluated arg
  // Bind() will then bind these values to the arguments
  // and store the old environment in the space allocated here
  bool save = base->save;

  while(pfargs)
  {
    LISP evarg = iEval(car(pfargs));

    if(save)
      PushF(NIL);                      // Space for symbol pointer

    PushF(evarg);
    base->nargs++;
    if(base->nargs == 0)               // Oops! we wrapped around
      LispError(ERR_NUM_FORMAL);

    pfargs = cdr(pfargs);
  }
}

// Push a list of args onto the stack
void PushArgs(LISP fargs)
{
  // If this is an arg frame just push each arg
  // If this is a save frame create saved arg entries
  // which are NULL but for the saved value of the arg
  // Bind() will then bind these values to the arguments
  // and store the old environment in the space allocated here
  bool save = base->save;

  while(fargs)
  {
    LISP arg = car(fargs);

    if(save)
      PushF(NIL);                      // Space for symbol pointer

    PushF(arg);
    base->nargs++;
    if(base->nargs == 0)               // Oops! we wrapped around
      LispError(ERR_NUM_FORMAL);

    fargs = cdr(fargs);
  }
}

// Map eval over the list of args
// This has nothing to do with the stack; its just here
// with the other argument handling functions
LISP EvList(LISP args)
{
  protect(pargs, args);
  protect(result, NewCons(NIL, NIL));
  protect(tail, result);

  while(pargs)
  {
    // NewCons() must complete before cdr() starts
    // so put a sequence point between
    LISP newcdr = NewCons(iEval(car(pargs)), NIL);
    cdr(tail) = newcdr;
    tail = cdr(tail);
    pargs = cdr(pargs);
  }
  return cdr(result);
}

// Bind a list of formal args to their evaluated values
// in the frame and save the old environment into the frame
void Bind(LISP formals)
{
  LISP *args = base->args;
  LISP formal;
  byte nargs = base->nargs;

  while(formals && nargs)
  {
    formal = car(formals);
    formals = cdr(formals);
    // Handle list (defaulted) formal arguments
    if(IsCons(formal))
      formal = car(formal);
    if(!IsSymbol(formal))
      LispError(ERR_BAD_FORMAL);
    *args++ = formal;
    // Get current value
    LISP oldval = value(formal);

    // Store new value
    value(formal) = *args;
    // Save the old value
    *args++ = oldval;
    nargs--;
  }
  if(nargs)
    LispError(ERR_NUM_ARGS);

  if(!formals)
    return;
  // Now deal with any optional arguments
  protect(pformals, formals);

  while(pformals)
  {
    formal = car(pformals);
    // Must be an optional formal arg
    if(!IsCons(formal))
      LispError(ERR_NUM_ARGS);

    if(!IsSymbol(car(formal)))
      LispError(ERR_BAD_FORMAL);

    // Save in frame
    PushF(car(formal));
    PushF(value(car(formal)));

    // Evaluate and store default
    LISP val = iEval(cdr(formal));
    value(caar(pformals)) = val;

    base->nargs++;
    if(base->nargs == 0)              // Oops! we wrapped around
      LispError(ERR_NUM_FORMAL);

    pformals = cdr(pformals);
  }
}

// Sweep the stack moving all cells referenced to new heap
// Also relocates protected pointers
//
// The Stack frames look like this:
//
//   +-------------------+
//   | saved arg         | ----> CELL
//   | saved arg         | ----> CELL
//   | ...               |
//   | function pointer  | ----> CELL
//   +-------------------+
//   | frame header      |
//   +-------------------+
//   Every line of the above diagram is a longword
//   The frame header should not be adjusted at all
//   The saved args should be moved
//   The size of the saved args block is:
//   (nargs + 1) * sizeof(LISP) for an arg frame and
//   (nargs * 2 + 1) * sizeof(LISP) for a save frame
//   The +1 includes the function pointer
void SweepStack()
{
  if(stop == stack)
    return;                     // Nothing to do

  PFrame sbase = base;
  LISP* ftop = (LISP*)stop;
  while(true)
  {
    while(ftop > &sbase->fun)
      MoveCell(*--ftop);
    // Last is zero in the root frame
    if(sbase->last == 0)
      break;
    // Step back to previous frame
    ftop = (LISP*)sbase;
    sbase = (PFrame)((pointer) sbase - sbase->last);
  }
}

// Restore the top-level environment
void ClearStack()
{
  while(stop > stack)
    PopFrame();
}

// Clear down the stack and print a stack trace if backtrace > 0
void BacktraceDump(int n /* = 0 */)
{
  if(n == 0)
  {
    n = backtrace;

    if(n > 0)
      fwrites(stderr, "\nBacktrace\n");
  }

  pointer s = stop;
  PFrame b = base;

  while(s > stack)
  {
    if(n-- > 0)
    {
      fwrites(stderr, "Eval   : ");
      print(b->fun, stderr, false);
      fwritec(stderr, '\n');
      for(int i = 0; i < b->nargs; i++)
      {
        // Print the arguments for...
        if(b->save)
        {
          // ...Lambda expressions
          fwritef(stderr, "  Arg%-2d: ", i + 1);

          // Error may have occurred while binding args
          if(!b->args[i * 2])
            fwrites(stderr, "UNBOUND\n");
          else
          {
            print(b->args[i * 2], stderr, false);
            fwrites(stderr, " = ");
            print(value(b->args[i << 1]), stderr, false);
            fwritec(stderr, '\n');
          }
        }
        else
        {
          //...and primitive functions
          fwritef(stderr, " Arg%-2d = ", i + 1);
          print(b->args[i], stderr, false);
          fwritec(stderr, '\n');
        }
      }
      // Then print any locals declared
      LISP* locals = b->args + b->nargs;
      if(b->save)
        locals += b->nargs;

      while(locals < (LISP*)s)
      {
        if(*locals)
        {
          fwrites(stderr, " Local : ");
          print(*locals, stderr, false);
          fwrites(stderr, " = ");
          print(value(*locals), stderr, false);
          fwritec(stderr, '\n');
        }
        locals += 2;
      }
      if(locals != (LISP*)s)
        fwrites(stderr, "*** This frame is damaged ***");
    }
    s = (pointer) b;               // Restore old stack top
    b = (PFrame) (s - b->last);    // Restore old frame base
  }
}



