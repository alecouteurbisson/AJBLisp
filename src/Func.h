////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// func.h
////////////////////////////////////////////////////////////////////////////
//
// Almost all of these functions implement the Lisp function of the same name
//
// Functions prefixed with 'i' are (mostly!) internal versions of the Lisp
// function with no argument type checking (caller beware!) and fixed arity.
//
#ifndef funcH
#define funcH

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <time.h>

//---------------------------------------------------------------------------
// Now some stuff to reduce the clutter
//---------------------------------------------------------------------------
// Argument references
#define ARG1 args[0]
#define ARG2 args[1]
#define ARG3 args[2]
#define ARG4 args[3]
#define ARG5 args[4]
#define ARG6 args[5]
#define ARG7 args[6]

// Function arity checking
#define CHECK_ARITY_EQ(n) \
  if(nargs != n) return LispError(ERR_NUM_ARGS)

#define CHECK_ARITY_IN(n, m) \
  if((nargs < n) || (nargs > m)) return LispError(ERR_NUM_ARGS)

#define CHECK_ARITY_LE(n) \
  if(nargs > n) return LispError(ERR_NUM_ARGS)

// Argument type checking
#define EXPECT_ATOM(a) \
  if(!IsAtom(a)) return LispError(ERR_ATOM_EXP, a)

#define EXPECT_LIST(a) \
  if(!IsList(a)) return LispError(ERR_LIST_EXP, a)

#define EXPECT_LIST2(a, b) \
  if(!IsList(a)) return LispError(ERR_LIST_EXP, a); \
  if(!IsList(b)) return LispError(ERR_LIST_EXP, b)

#define EXPECT_CONS(a) \
  if(!IsCons(a)) return LispError(ERR_LIST_EXP, a)

#define EXPECT_SYM(a) \
  if(!IsSymbol(a)) return LispError(ERR_SYM_EXP, a)

#define EXPECT_SYM2(a, b) \
  if(!IsSymbol(a)) return LispError(ERR_SYM_EXP, a); \
  if(!IsSymbol(b)) return LispError(ERR_SYM_EXP, b)

#define EXPECT_KEY(a) \
  if(!IsKey(a)) return LispError(ERR_KEY_EXP, a)

#define EXPECT_NUM(a) \
  if(!IsNumber(a)) return LispError(ERR_NUM_EXP, a)

#define EXPECT_NUM2(a, b) \
  if(!IsNumber(a)) return LispError(ERR_NUM_EXP, a); \
  if(!IsNumber(b)) return LispError(ERR_NUM_EXP, b)

#define EXPECT_INT(a) \
  if(!IsInteger(a)) return LispError(ERR_INT_EXP, a)

#define EXPECT_INT2(a, b) \
  if(!IsInteger(a)) return LispError(ERR_INT_EXP, a); \
  if(!IsInteger(b)) return LispError(ERR_INT_EXP, b)

#define EXPECT_REAL(a) \
  if(!IsReal(a)) return LispError(ERR_REAL_EXP, a)

#define EXPECT_REAL3(a, b, c) \
  if(!IsReal(a)) return LispError(ERR_REAL_EXP, a); \
  if(!IsReal(b)) return LispError(ERR_REAL_EXP, b); \
  if(!IsReal(c)) return LispError(ERR_REAL_EXP, c)

#define EXPECT_CHR(a) \
  if(!IsChar(a)) return LispError(ERR_CHR_EXP, a)

#define EXPECT_CHR2(a, b) \
  if(!IsChar(a)) return LispError(ERR_CHR_EXP, a); \
  if(!IsChar(b)) return LispError(ERR_CHR_EXP, b)

#define EXPECT_STR(a) \
  if(!IsString(a)) return LispError(ERR_STR_EXP, a)

#define EXPECT_STR2(a, b) \
  if(!IsString(a)) return LispError(ERR_STR_EXP, a); \
  if(!IsString(b)) return LispError(ERR_STR_EXP, b)

#define EXPECT_FILE(a) \
  if(!IsFile(a)) return LispError(ERR_FILE_EXP, a)

#define EXPECT_VEC(a) \
  if(!IsVector(a)) return LispError(ERR_VEC_EXP, a)

#define EXPECT_WIND(a) \
  if(!IsWindow(a)) return LispError(ERR_WIN_EXP, a)

//#############################################################################
// Basic primitives
//#############################################################################
LISP Quote(byte nargs, LISP args[]);
LISP SymbolValue(byte nargs, LISP args[]);
LISP Cons(byte nargs, LISP args[]);
LISP Set(byte nargs, LISP args[]);
LISP iSet(LISP c, LISP value);
LISP Setq(LISP& args);
LISP Psetq(LISP& args);
LISP Car(byte nargs, LISP args[]);
LISP Cdr(byte nargs, LISP args[]);
LISP Caar(byte nargs, LISP args[]);
LISP Cadr(byte nargs, LISP args[]);
LISP Cdar(byte nargs, LISP args[]);
LISP Cddr(byte nargs, LISP args[]);
LISP Caaar(byte nargs, LISP args[]);
LISP Caadr(byte nargs, LISP args[]);
LISP Cadar(byte nargs, LISP args[]);
LISP Caddr(byte nargs, LISP args[]);
LISP Cdaar(byte nargs, LISP args[]);
LISP Cdadr(byte nargs, LISP args[]);
LISP Cddar(byte nargs, LISP args[]);
LISP Cdddr(byte nargs, LISP args[]);
LISP Rplaca(byte nargs, LISP args[]);
LISP Rplacd(byte nargs, LISP args[]);
LISP Eval(byte nargs, LISP args[]);
LISP Apply(byte nargs, LISP args[]);
LISP Typeof(byte nargs, LISP args[]);
LISP Collect(byte nargs, LISP args[]);

//#############################################################################
// Symbol functions
//#############################################################################
LISP Symbol(byte nargs, LISP args[]);
LISP Free(byte nargs, LISP args[]);
LISP Plist(byte nargs, LISP args[]);
LISP Put(byte nargs, LISP args[]);
LISP Get(byte nargs, LISP args[]);
LISP Remprop(byte nargs, LISP args[]);
LISP DefConstant(LISP& args);
LISP DefKey(LISP& args);
LISP Destroy(LISP& args);
LISP Oblist(byte nargs, LISP args[]);
LISP Let(LISP& args);
LISP Slet(LISP& args);
LISP Gensym(byte nargs, LISP args[]);

//#############################################################################
// List functions
//#############################################################################
LISP List(LISP& args);
LISP Length(byte nargs, LISP args[]);
int  iLength(LISP l);
LISP Nth(byte nargs, LISP args[]);
LISP NthCdr(byte nargs, LISP args[]);
LISP iNth(byte nargs, LISP args[], bool rtncdr);
LISP Last(byte nargs, LISP args[]);
LISP Append(LISP& args);
LISP Delete(byte nargs, LISP args[]);
LISP Reverse(byte nargs, LISP args[]);
LISP Assoc(byte nargs, LISP args[]);
LISP CdrAssoc(byte nargs, LISP args[]);
LISP iAssoc(LISP x, LISP a, LERROR err);
LISP Member(byte nargs, LISP args[]);
LISP iMember(LISP a, LISP b);
LISP Filter(byte nargs, LISP args[]);
LISP Mapc(byte nargs, LISP args[]);
LISP Push(byte nargs, LISP args[]);
LISP Pop(byte nargs, LISP args[]);

//#############################################################################
// Vector functions
//#############################################################################
LISP MakeVector(byte nargs, LISP args[]);
LISP Vector(byte nargs, LISP args[]);
LISP iVector(LISP l);
LISP Vlist(byte nargs, LISP args[]);
LISP Vref(byte nargs, LISP args[]);
LISP Vset(byte nargs, LISP args[]);
LISP Vsort(byte nargs, LISP args[]);
int _USERENTRY iSortLexCmp(const void*a, const void*b);
int _USERENTRY iSortUserCmp(const void* a, const void* b);

//#############################################################################
// Character functions
//#############################################################################
LISP Char(byte nargs, LISP args[]);
LISP CharEqual(byte nargs, LISP args[]);
LISP CharNEqual(byte nargs, LISP args[]);
LISP CharGreater(byte nargs, LISP args[]);
LISP CharLess(byte nargs, LISP args[]);
LISP CharNGreater(byte nargs, LISP args[]);
LISP CharNLess(byte nargs, LISP args[]);
int  iCharCompare(byte nargs, LISP args[]);
LISP UpperCasep(byte nargs, LISP args[]);
LISP LowerCasep(byte nargs, LISP args[]);

//#############################################################################
// String functions
//#############################################################################
LISP String(byte nargs, LISP args[]);
LISP Chars(byte nargs, LISP args[]);
LISP Schar(byte nargs, LISP args[]);
LISP StringEqual(byte nargs, LISP args[]);
LISP StringNEqual(byte nargs, LISP args[]);
LISP StringGreater(byte nargs, LISP args[]);
LISP StringLess(byte nargs, LISP args[]);
LISP StringNGreater(byte nargs, LISP args[]);
LISP StringNLess(byte nargs, LISP args[]);
int  iStringCompare(byte nargs, LISP args[]);
LISP UpperCase(byte nargs, LISP args[]);
LISP LowerCase(byte nargs, LISP args[]);
LISP StringSearch(byte nargs, LISP args[]);
LISP Concat(LISP& args);
LISP Substring(byte nargs, LISP args[]);
LISP iToken(LISP arg, int& i);
LISP Tokenise(byte nargs, LISP args[]);

//#############################################################################
// Date/Time functions
//#############################################################################
LISP Locale(byte nargs, LISP args[]);
LISP Tzset(byte nargs, LISP args[]);
LISP LocalTime(byte nargs, LISP args[]);
LISP Dst(byte nargs, LISP args[]);
LISP Now(LISP& args);
tm*  iConvertTime(time_t itime);
tm*  iTimeArgs(byte nargs, LISP args[]);
LISP Date(byte nargs, LISP args[]);
LISP Time(byte nargs, LISP args[]);
LISP EncodeTime(byte nargs, LISP args[]);
time_t iEncodeTime(int y, int m, int d, int H = 0, int M = 0, int S = 0);
LISP DateTime(byte nargs, LISP args[]);
LISP TimeString(byte nargs, LISP args[]);
//void iSummerTime(tm*& dectime);
bool iSummerTime(tm*& dectime, time_t itime);
time_t iDecodeChangeOverTime(SYSTEMTIME& st, int year);
LISP Timer(LISP& args);

//#############################################################################
// Stream functions
//#############################################################################
LISP OpenFile(byte nargs, LISP args[]);
LISP CloseFile(byte nargs, LISP args[]);
LISP FilePosition(byte nargs, LISP args[]);
LISP FloatFormat(byte nargs, LISP args[]);
LISP IntegerFormat(byte nargs, LISP args[]);
LISP iPrint(LISP args, bool cflag, bool cr);
LISP iPrint(FILE* out, LISP args, bool cflag, bool cr);
LISP Write(LISP& args);
LISP Print(LISP& args);
LISP Writeln(LISP& args);
LISP Println(LISP& args);
LISP PrintFormat(LISP& args);
void iFormat(FILE* f, char* fmt, LISP args);
LISP Read(byte nargs, LISP args[]);
LISP Readline(byte nargs, LISP args[]);
LISP Readchar(byte nargs, LISP args[]);
LISP Writechar(byte nargs, LISP args[]);
LISP Readbyte(byte nargs, LISP args[]);
LISP Writebyte(byte nargs, LISP args[]);
LISP Readword(byte nargs, LISP args[]);
LISP Writeword(byte nargs, LISP args[]);
LISP Readlong(byte nargs, LISP args[]);
LISP Writelong(byte nargs, LISP args[]);
LISP Unread(byte nargs, LISP args[]);
LISP Eof(byte nargs, LISP args[]);
LISP Flush(byte nargs, LISP args[]);
LISP Load(byte nargs, LISP args[]);
FILE* iOpenLispSource(char* fname);
LISP LogFile(byte nargs, LISP args[]);
void Spaces(FILE* f, int n);
LISP PrintSize(LISP& args);
LISP WriteSize(LISP& args);
int  iPrintSize(LISP args, int sz, bool cflag);
int  iPrintSize1(LISP arg, int sz, bool cflag);

//#############################################################################
// Predicates
//#############################################################################
LISP Eq(byte nargs, LISP args[]);
LISP iEq(LISP a, LISP b);
LISP Eql(byte nargs, LISP args[]);
LISP iEql(LISP a, LISP b);
LISP Equal(byte nargs, LISP args[]);
LISP iEqual(LISP a, LISP b);
LISP Zerop(byte nargs, LISP args[]);
LISP Onep(byte nargs, LISP args[]);
LISP Plusp(byte nargs, LISP args[]);
LISP Minusp(byte nargs, LISP args[]);
LISP Atom(byte nargs, LISP args[]);
LISP Symbolp(byte nargs, LISP args[]);
LISP Constantp(byte nargs, LISP args[]);
LISP Keyp(byte nargs, LISP args[]);
LISP Variablep(byte nargs, LISP args[]);
LISP Numberp(byte nargs, LISP args[]);
LISP Integerp(byte nargs, LISP args[]);
LISP Floatp(byte nargs, LISP args[]);
LISP Listp(byte nargs, LISP args[]);
LISP Consp(byte nargs, LISP args[]);
LISP Charp(byte nargs, LISP args[]);
LISP Stringp(byte nargs, LISP args[]);
LISP Subrp(byte nargs, LISP args[]);
LISP Execp(byte nargs, LISP args[]);
LISP FSubrp(byte nargs, LISP args[]);
LISP Vectorp(byte nargs, LISP args[]);
LISP Filep(byte nargs, LISP args[]);
LISP Windowp(byte nargs, LISP args[]);
LISP Freep(byte nargs, LISP args[]);
LISP Typep(byte nargs, LISP args[]);
LISP LexCmp(byte nargs, LISP args[]);
int  iLexCmp(LISP arg1, LISP arg2);

//#############################################################################
// Arithmetic operators
//#############################################################################
LISP Add(LISP& args);
LISP Subtract(byte nargs, LISP args[]);
LISP Multiply(LISP& args);
LISP Divide(byte nargs, LISP args[]);
LISP Rem(byte nargs, LISP args[]);
LISP Inc(byte nargs, LISP args[]);
LISP Dec(byte nargs, LISP args[]);


//#############################################################################
// Arithmetic predicates
//#############################################################################
LISP Greaterp(byte nargs, LISP args[]);
LISP Lessp(byte nargs, LISP args[]);
LISP NGreaterp(byte nargs, LISP args[]);
LISP NLessp(byte nargs, LISP args[]);
LISP Equalp(byte nargs, LISP args[]);
LISP NEqualp(byte nargs, LISP args[]);
double iCompare(byte nargs, LISP args[], LISP& err);

//#############################################################################
// Mathematical functions
//#############################################################################
double MathsArgs(byte nargs, LISP args[]);
LISP Abs(byte nargs, LISP args[]);
LISP Sign(byte nargs, LISP args[]);
LISP Float(byte nargs, LISP args[]);
LISP Integer(byte nargs, LISP args[]);
LISP Floor(byte nargs, LISP args[]);
LISP Ceil(byte nargs, LISP args[]);
LISP Sqrt(byte nargs, LISP args[]);
LISP Sin(byte nargs, LISP args[]);
LISP Cos(byte nargs, LISP args[]);
LISP Tan(byte nargs, LISP args[]);
LISP Sinh(byte nargs, LISP args[]);
LISP Cosh(byte nargs, LISP args[]);
LISP Tanh(byte nargs, LISP args[]);
LISP Asin(byte nargs, LISP args[]);
LISP Acos(byte nargs, LISP args[]);
LISP Atan(byte nargs, LISP args[]);
LISP Asinh(byte nargs, LISP args[]);
LISP Acosh(byte nargs, LISP args[]);
LISP Atanh(byte nargs, LISP args[]);
LISP Log(byte nargs, LISP args[]);
LISP Ln(byte nargs, LISP args[]);
LISP Exp(byte nargs, LISP args[]);
LISP Power(byte nargs, LISP args[]);
LISP Random(byte nargs, LISP args[]);

//#############################################################################
// Boolean operators
//#############################################################################
LISP Not(byte nargs, LISP args[]);
LISP And(LISP& args);
LISP Or(LISP& args);
LISP Xor(LISP& args);

//#############################################################################
// Bitwise logical operators
//#############################################################################
LISP Bnot(byte nargs, LISP args[]);
LISP Band(LISP& args);
LISP Bor(LISP& args);
LISP Bxor(LISP& args);
LISP Bset(byte nargs, LISP args[]);
LISP Bclr(byte nargs, LISP args[]);
LISP Btgl(byte nargs, LISP args[]);
LISP Btst(byte nargs, LISP args[]);
LISP Bcnt(byte nargs, LISP args[]);
LISP Bshift(byte nargs, LISP args[]);
LISP Ashift(byte nargs, LISP args[]);

//#############################################################################
// Control flow
//#############################################################################
LISP If(byte nargs, LISP args[]);
LISP Cond(LISP& args);
LISP For(LISP& args);
LISP ForEach(LISP& args);
LISP Loop(LISP& args);
LISP Until(LISP& args);
LISP While(LISP& args);
LISP Break(LISP& args);
LISP Progn(LISP& args);
LISP Prog1(LISP& args);
LISP When(LISP& args);
LISP Unless(LISP& args);
LISP Case(LISP& args);

//#############################################################################
// Errors and exceptions
//#############################################################################
LISP Error(byte nargs, LISP args[]);
LISP Errorset(LISP& args);
LISP ErrorMsg(byte nargs, LISP args[]);
LISP Guard(LISP& args);
LISP Catch(LISP& args);
LISP Throw(byte nargs, LISP args[]);
LISP Backtrace(byte nargs, LISP args[]);
LISP Exit(LISP&);

//#############################################################################
// System functions
//#############################################################################
LISP lDeleteFile(byte nargs, LISP args[]);
LISP lMoveFile(byte nargs, LISP args[]);
LISP lCopyFile(byte nargs, LISP args[]);
LISP ProtectFile(byte nargs, LISP args[]);
LISP lCreateDirectory(byte nargs, LISP args[]);
LISP lDeleteDirectory(byte nargs, LISP args[]);
LISP Dir(byte nargs, LISP args[]);
LISP iFileInfo(WIN32_FIND_DATA& filedata);
int iFileTimeConvert(FILETIME& ft);
LISP Cd(byte nargs, LISP args[]);
LISP lSystem(byte nargs, LISP args[]);
LISP lSleep(byte nargs, LISP args[]);
LISP Scripted(LISP& args);
LISP Environ(byte nargs, LISP args[]);
LISP Status(byte nargs, LISP args[]);

//#############################################################################
// Graphics functions
//#############################################################################
// These are in gfunc.cpp
void GGetCoords(LISP w, LISP a1, LISP a2, int& x, int& y);
void GGetVector(LISP w, LISP a1, LISP a2, int& x, int& y);
void GGetNum2(LISP a1, LISP a2, double& x, double& y);
LISP GWindow(byte nargs, LISP args[]);
LISP GClose(byte nargs, LISP args[]);
LISP GClear(byte nargs, LISP args[]);
LISP GMove(byte nargs, LISP args[]);
LISP GPos(byte nargs, LISP args[]);
LISP GDraw(byte nargs, LISP args[]);
LISP GPoint(byte nargs, LISP args[]);
LISP GPick(byte nargs, LISP args[]);
LISP GEllipse(byte nargs, LISP args[]);
LISP GRectangle(byte nargs, LISP args[]);
LISP GText(byte nargs, LISP args[]);
LISP GTextSize(byte nargs, LISP args[]);
LISP GViewport(byte nargs, LISP args[]);
LISP GVTrans2(byte nargs, LISP args[]);
LISP GPen(byte nargs, LISP args[]);
LISP GBrush(byte nargs, LISP args[]);
LISP GColour(byte nargs, LISP args[]);
LISP GFont(byte nargs, LISP args[]);

//#############################################################################
// Debug and instrumentation
//#############################################################################
LISP Debug(byte nargs, LISP args[]);
LISP Addr(byte nargs, LISP args[]);
LISP Dump(LISP&);
LISP DumpHash(LISP&);
LISP Statistics(LISP& arg);

extern char *ErrorMessage;
//---------------------------------------------------------------------------
#endif
