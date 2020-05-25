////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// io.cpp
//
// Stream I/O for AJBLisp
////////////////////////////////////////////////////////////////////////////
#include "AJBLisp.h"
#include "Listenerif.h"
#include <conio.h>
#include <float.h>
//---------------------------------------------------------------------------
// Formats used for print
char ifmt[maxfmt] = "%#1ld";
char ffmt[maxfmt] = "%#1.8lg";

// Fixed formats used for write
char *wifmt = "%ld";
// Note that I should be able to use "%#1g" to get only the significant
// figures but this is broken and defaults to 6 figures
char *wffmt =  "%#1.16lg";

// Input buffer
const static int BUFSZ = 512;
static char buffer[BUFSZ];

FILE* logfile = 0L;

//---------------------------------------------------------------------------
// High level I/o
// These functions handle the formatting of text output and the parsing
// of text input

// Read an atom
// Arranged as a series of tests for each type with anything that
// fails to parse falling through to the next test with the symbol parser
// accepting anything not otherwise recognised.  This means that, in
// principle, any input is valid but the syntax of escape characters must be
// correct and symbols begining with '#' are reserved for special purposes
LISP ratom(FILE* in)
{
  char* p = buffer;             // Buffer pointer
  int c;

  c = inch(in);
  if(c == '#')                  // Read macro character
  {
    c = inch(in);
    if(c == '\\')               // #\... is a character
      return rchar(in);

    if(c == '(')                // #( ... ) is a literal vector
    {
      uninch(c, in);
      LISP v = iVector(read(in));
      // flags(v) = tCVECT;        // Make readonly
      return v;
    }

    // Note that the form #<...> used for files, subrs etc, is illegal input
    if(c == '<')
      LispError(ERR_UNREADABLE);
    LispError(ERR_BAD_RMAC);
  }

  while(true)
  {
    // End of symbol?
    if(isspace(c) || (c == '(') || (c == ')') || (c == EOF) || (c == ';'))
      break;
    if(c == '\\')               // Escape character
    {
      c = inch(in);
      if(c == EOF)
        LispError(ERR_EOF_ESC); // EOF after reading escape character
    }
    *p++ = (char)c;
    if((p - buffer - 1) == BUFSZ)
      LispError(ERR_TOK_LEN);
    c = inch(in);
  }
  uninch(c, in);
  *p = '\0';

  // Now check the token in the buffer to determine if it is an integer
  unsigned int i;
  char* end;                    // The last character converted
  bool neg = false;
  char* b = buffer;
  while((*b == '-') || (*b == '+'))   // Strip signs
  {
    if(*b == '-')               // Keep the product of the - signs
      neg = !neg;
    b++;
  }
  if(*b)
  {
    i = strtoul(b, &end, 0);
    // Allow signed or unsigned values
    if(!(*end || (neg && (i > 0x7FFFFFFF))))
      return(NewInteger(neg ? -(int)i : (int)i));
  }
  // Maybe it is a real
  double r;
  r = strtod(buffer, &end);
  if(!*end)
    return(NewReal(r));

  // Must be a symbol
#ifdef UPPER_CASE_SYMBOLS
  if(!stricmp(buffer, "NIL")) return NIL;
#else
  if(!strcmp(buffer, "nil")) return NIL;
#endif
  // Does this symbol exist?
  LISP s = FindSym(buffer);
  if(s) return s;

  // No, create a new one
  return NewSym(buffer);
}

// Read a character, #\ has been read already
// #\A     is 'A'
// #\^A    is Control A
// #\^     is '^'
// #\^65   is 'A'      (ASCII decimal)
// #\^0x41 is 'A'      (ASCII hex)
LISP rchar(FILE* in)
{
  int i = 1;

  buffer[0] = (char)inch(in);
  if(buffer[0] == '^')          // ^ found, read to end of token
  {
    for( ; i < 10; i++)
    {
      int c = inch(in);
      if(isspace(c) || (c == '(') || (c == ')') || (c == EOF) || (c == ';'))
      {
        uninch(c, in);
        buffer[i] = '\0';
        break;
      }
      else
      {
        buffer[i] = (char)c;
      }
    }
  }

  // Sanity check
  if(i >= 10)
    LispError(ERR_BAD_RMAC);

  if(buffer[0] == '^')
  {
    if(i == 1)
      return NewChar('^');

    // Escaped '^'?
    //if((i == 2) && (buffer[1] == '^'))
    //  return NewChar('^');

    // Control - literal?
    if((i == 2) && (buffer[1] >= '@') && (buffer[1] <= '~'))
      return NewChar((char)(buffer[1] - '@'));
    char *end;

    // Must be a literal integer ASCII value
    unsigned long n = strtoul(buffer + 1, &end, 0);
    if((end != buffer + i) || (n > 255) || (buffer[1] == '\0'))
      LispError(ERR_BAD_RMAC);
    return NewChar((char)n);
  }

  // Literal ASCII char
  if((i == 1) && isprint(buffer[0]))
    return(NewChar(buffer[0]));
  else
    LispError(ERR_BAD_RMAC);
  return NIL;
}

// Read a string processing escape characters
LISP rstring(FILE* in)
{
  int  bufp = 0;
  int c;
  do
  {
    c = inch(in);
    if(c == EOF)
      LispError(ERR_EOF_STR);   // EOF inside string
    if(c < ' ')
      LispError(ERR_STR_CHR);   // Illegal character inside string
    if(c == '\"') c = '\0';
    if(c == '\\')
    {
      c = inch(in);
      if(c == EOF)
        LispError(ERR_EOF_STR); // EOF inside string
      if(c == 'n') c = '\n';
      if(c == 'r') c = '\r';
      if(c == 't') c = '\t';
    }
    // Now store the character into the buffer
    if(bufp >= BUFSZ)
      LispError(ERR_STR_LEN);   // Out of string space

    buffer[bufp++] = (char)c;
  } while(c);

  LISP s = NewString(strlen(buffer) + 1);
  strcpy(text(s), buffer);
  return s;
}

// Skip all leading whitespace and comments and return the next
// input character or EOF
int skipspace(FILE* in)
{
  int c;
  do
  {
    c = inch(in);
    if(c == EOF) return EOF;

    // Comment?
    if(c == ';')
    {
      do
      {
        c = inch(in);
        if(c == EOF) return EOF;
      } while((c != '\n') && (c != '\r'));
    }
  } while(isspace(c));
  return c;
}

// Read an s-expression from the listener for the
// REPL checking for system commands
LISP replread()
{
  int c;

  // If REPL input starts with a ! then the rest of the line
  // is passed to the system command processor.  The output,
  // if any, is written to the terminal.
  // Escaping the ! with \ will override this
  // e.g.
  // > !dir  ; print a directory listing
  // > \!dir ; return the value of the symbol !dir

  c = inch(stdin);
  if(c == '!')
  {
    LISP line = NewString(treadln());
    LISP result = NewCons(line, NIL);
    result = NewCons(FindSym("system"), result);
    return result;
  }
  uninch(c, stdin);

  return read(stdin);
}

// Read an s-expression
LISP read(FILE* in)
{
  LISP temp;

  int c;

  c = skipspace(in);

  switch(c)
  {
    case EOF:
      return EOF_;

    case '(':
      temp = (readlist(in));
      return temp;

    case ')':
      LispError(ERR_BAD_RPAR);

    case '\'':
      temp = read(in);
      temp = NewCons(temp, NIL);
      return NewCons(QUOTE, temp);

    case '"':
      temp = rstring(in);
      return temp;

    default:
      uninch(c, in);
      return ratom(in);
  }
}

// An open parenthesis has been read, read the rest of the list
LISP readlist(FILE* in, bool dot_ok /* = false */)
{
  int c;
  c = skipspace(in);

  if(c == EOF)
    LispError(ERR_EOF_LIST);    // EOF inside list

  if (c == ')')
    return(NIL);

  // Read the next item in the list
  uninch(c, in);
  protect(t, NIL);
  t = read(in);

  // If it's a dot then the list must terminate after the next (required) item
  if(t == DOT)
  {
    if(!dot_ok)
      LispError(ERR_DOT_SYNTAX);

    // Read the next item
    t = read(in);

    // Check for close paren
    c = skipspace(in);
    if(c == EOF)
      LispError(ERR_EOF_LIST);  // EOF inside list
    if (c != ')')
      LispError(ERR_NO_RPAR);   // Missing close parenthesis
    return(t);
  }
  // dots allowed now we have read a first element
  LISP temp = readlist(in, true);
  return NewCons(t,temp);
}

// Flush terminal or file buffers
LISP flush(FILE* file)
{
  if(file == stdin)
  {
    // flush terminal input buffer
    tflushin();
  }
  else if(file == stdout)
  {
    // flush terminal output buffer
    tflushout();
  }
  else
    fflush(file);
  return NIL;
}

// Read input line verbatim as a string
LISP readline(FILE* in)
{
  const char *line = readln(in);
  return NewString(line);
}

// In all of the following 'cflag' is set if Print or Println was called
// and reset if Write or Writeln was called

// patom() outputs a printable representation of an atom to a stream
// The output formats are as follows
// Symbol      Output name
// String      Output string value
// Integer     Output value in text form
// Real        Output value in text form
// Subr        Output as #<Subr:address>
// FSubr       Output as #<FSubr:address>
// Subrl       Output as #<Subrl:address>
// FSubrl      Output as #<FSubrl:address>
// File        Output as #<File:address>
// Vector      Output as #(<content>)
//
// Yes, I know a vector is not an atom but this is a good place to handle
// the printing of vectors
//
// <address> is the execution address of the code or the
// file handle as eight hex digits.
// e.g. <Subr#002A37F5>
// This is really only useful for debugging the interpreter.
// (Why else are you printing F/Subr atoms anyway?)
//
// Subrs, FSubrs, Windows and Files cannot be read back in.
// The prefix '#<' identifies these as unreadable atoms to the
// input parser, ratom().
LISP patom(LISP c, FILE* out, bool cflag)
{
  if(!c)
#ifdef UPPER_CASE_SYMBOLS
    fwrites(out, "NIL");
#else
    fwrites(out, "nil");
#endif
  else
    switch(c->flags)
    {
      case tCONS:
        LispError(ERR_PATOM);
      break;

      case tSYM:
      case tCONST:
      case tKEY:
        if(cflag)
          fwrites(out, name(c), c->size - SYM_SIZE);
        else
        {
          for(integer i = 0; i < (c->size - SYM_SIZE); i++)
          {
            char ch = name(c)[i];
            // Escape special characters
            if(isspace(ch) ||
               (ch == '(') ||
               (ch == ')') ||
               (ch == ';') ||
               (ch == '"') ||
               ((i == 0) && ((ch == '#') || (ch == '\''))))
              fwritec(out, '\\');
            fwritec(out, ch);
          }
        }
      break;

      case tCHR:
        if(cflag)
        {
          fwritec(out, chr(c));
        }
        else
        {
          // Note '^' is just #\^ now
          if(isprint(chr(c)) && !isspace(chr(c)))
            fwritef(out, "#\\%c", chr(c));
          else
            fwritef(out, "#\\^%u", (unsigned char)chr(c));
        }
      break;

      case tSTR:
        if(!cflag)
          fwritec(out, '"');

        if(cflag)
          fwrites(out, text(c));
        else
          for(int i = 0; text(c)[i]; i++)
          {
            switch(text(c)[i])
            {
              case '\n': fwrites(out, "\\n");
              break;

              case '\r': fwrites(out, "\\r");
              break;

              case '\t': fwrites(out, "\\t");
              break;

              case '\"': fwrites(out, "\\\"");
              break;

              case '\\': fwrites(out, "\\\\");
              break;

              default  : fwritec(out, text(c)[i]);
              break;
            }
          }

        if(!cflag)
          fwritec(out, '"');
      break;

      case tINT:
        if(!cflag)  // Ensure re-readable
          fwritef(out, wifmt, ivalue(c));
        else
          fwritef(out, ifmt, ivalue(c));
      break;

      case tREAL:
        if(!cflag)  // Ensure full precision used
          fwritef(out, wffmt, rvalue(c));
        else
          fwritef(out, ffmt, rvalue(c));
      break;

      case tSUBR:
        fwritef(out, "#<Subr:%p>", code(c));
      break;

      case tFSUBR:
        fwritef(out, "#<FSubr:%p>", code(c));
      break;

      case tSUBRL:
        fwritef(out, "#<Subrl:%p>", code(c));
      break;

      case tFSUBRL:
        fwritef(out, "#<FSubrl:%p>", code(c));
      break;

      case tFILE:
        fwritef(out, "#<File:%p>", file(c));
      break;

      case tWIND:
        fwritef(out, "#<Window:%p>", window(c));
      break;

      case tVECT:
      case tCVECT:
        fwrites(out, "#(");
        print(data(c)[0], out, cflag);
        for(long i = 1; i < dim(c); i++)
        {
          fwrites(out, " ");
          print(data(c)[i], out, cflag);
        }
        fwrites(out, ")");
      break;
    }
  return c;
}

// Print a list
void printlist(LISP c, FILE* out, bool cflag)
{
  while(true)
  {
    // Allow an escape from printing circular lists
    if(ThreadCheckTerminate())
      LispError(ERR_USER);

    print(car(c), out, cflag);
    c = cdr(c);
    if(!IsCons(c))
    {
      if(c)
      {
        fwrites(out, " . ");
        patom(c, out, cflag);
      }
      return;
    }
    fwrites(out, " ");
  }
}

// Print anything
LISP print(LISP c, FILE* out, bool cflag)
{
  if(!IsCons(c))
    return patom(c, out, cflag);
  fwritec(out, '(');
  printlist(c, out, cflag);
  fwritec(out, ')');
  return(c);
}

//---------------------------------------------------------------------------
// Low Level I/O
//---------------------------------------------------------------------------
// Note that these also write to the logfile if it is open and the primary
// destination is stdout.  Directly writing to the logfile is also handled
// correctly
// Be aware that if stdout and stderr actually go to different destinations
// then the output buffer must be flushed when switching streams.
//
// Output to stdout/stderr are handled as a special case. Data written
// directly to stdout, for example, will not appear on the terminal.
// In fact stdout is undefined since this is a GUI application.

// Write a character
void fwritec(FILE* file, char c)
{
  // Terminal output?
  bool term = (file == stderr) || (file == stdout);

  // If terminal output and logfile open
  if(term && logfile)
    putc(c, logfile);

  // Write to destination
  if(term)
    tfwritec(file, c);
  else
    putc(c, file);
}

// Write a character to stdout
void writec(char c)
{
  // If logfile open
  if(logfile)
    // Write to logfile
    putc(c, logfile);
  // Write to destination
  twritec(c);
}

// Write a string or buffer
// If len == -1 (default) then use strlen on buffer
void fwrites(FILE* file, char* c, int len)
{
  if(len < 0)
    len = strlen(c);

  // Terminal output?
  bool term = (file == stderr) || (file == stdout);

  // If terminal output and logfile open
  if(term && logfile)
    fwrite(c, len, 1, logfile);

  // Write to destination
  if(term)
    tfwrites(file, c, len);
  else
    fwrite(c, len, 1, file);
}

// Write a string or buffer to stdout
// If len == -1 (default) then use strlen on buffer
void writes(char* c, int len)
{
  if(len < 0)
    len = strlen(c);
  // If logfile open
  if(logfile)
    // Write to logfile
    fwrite(c, len, 1, logfile);
  // Write to destination
  twrites(c, len);
}

// Write formatted text
void fwritef(FILE* file, char* fmt, ...)
{
  va_list argptr;
  va_start(argptr, fmt);

  // Terminal output?
  bool term = (file == stderr) || (file == stdout);

  // If terminal output and logfile open
  if(term && logfile)
    // Write to logfile
    (void)vfprintf(logfile, fmt, argptr);

  // Write to destination
  if(term)
    tvfwritef(file, fmt, argptr);
  else
    (void)vfprintf(file, fmt, argptr);
  va_end(argptr);
}

// Write formatted text to stdout
void writef(char* fmt, ...)
{
  va_list argptr;

  va_start(argptr, fmt);

  // Write to logfile
  if(logfile)
    (void)vfprintf(logfile, fmt, argptr);

  // Write to destination
  tvfwritef(stdout, fmt, argptr);
  va_end(argptr);
}

// Read a character from the input
int inch(FILE* in)
{
  if(in == stdin)
    return tgetch();

  char c;
  int n = fread(&c, 1, 1, in);
  if(n == 0)
    return EOF;
  else
    return (int)c;
}

// Put a character back to the input
void uninch(int c, FILE* in)
{
  if(in == stdin)
    tungetc(c);
  else
    ungetc(c, in);
}

// Read a line of input
const char *readln(FILE* in)
{
  if(in == stdin)
    return treadln();

  static char buffer[512];
  int  bufp = 0;
  int c;
  do
  {
    c = inch(in);
    if((c == EOF) || (c == '\n') || (c == '\r'))
      c = '\0';
    // Now store the character into the buffer
    if(bufp >= 512)
    {
      LispError(ERR_STR_LEN);   // Out of string space
    }
    buffer[bufp++] = (char)c;
  } while(c);
  return buffer;
}


