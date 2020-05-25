////////////////////////////////////////////////////////////////////////////
//
// AJBLisp for Windows
//
// (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// Terminal.cpp
//
// Console I/O routines
////////////////////////////////////////////////////////////////////////////
//
// These routines handle console I/O and they try to behave intelligently
// with regard to prompting and incomplete input.
// When a complete expression is entered at the prompt then it is processed
// and the result is output (preceded by '=').
//
// When multiple complete inputs are entered on one line then each result is
// printed separated by a new line and preceeded by '='.  No unnecessary
// prompts are printed between the results.
//
// When the input line contains at least one complete expression line but
// ends with an an incomplete expression then after the results for the
// complete expressions have been printed the prompt will be printed followed
// by the incomplete expression ready for completion.
//
// Editing commands will not affect incomplete input since this has
// already been parsed.  You may edit the completion of the line however.
//
// Note that there is no way to return to the prompt when an incomplete input
// has been made without completing it (closing all open lists/strings) or
// causing a read error.
//
// The tf*(FILE* f, ... ) functions receive output destined for stdout and
// stderr.  Output to each is merged and printed normally at present.
#pragma hdrstop
#include "ajblisp.h"
#include "listenerif.h"
#define WIN32_LEAN_AND_MEAN
#include "windows.h"
#include <string.h>

#undef __USELOCALES__
#include <ctype.h>

// Buffered output functions
static void OutputChar(char c);
static void OutputString(char* c);
static void BufferedOutputString(char* c);

//---------------------------------------------------------------------------
#pragma package(smart_init)

// Number of lines of history kept
const int HISTORY = 25;

// format buffer
const int FBUFSZ = 512;
char  fmtbuf[FBUFSZ];
const char *FBUFEND = &fmtbuf[FBUFSZ - 1];

//////////////////////////////////////////////////////////////////////////////
// inbuf holds the input from the keyboard
// The pointers are set as follows
// inbuf: used input | incomplete input | unread input |\0
//                   ^startp            ^readp         ^endp
// startp is used to detect incomplete input so that it may be
// echoed to the user after the prompt has been printed
//
// All terminal output functions are buffered.
//
// Where the following code refers to output streams these are only stdout
// and stderr and all such output currently goes to the terminal.
//////////////////////////////////////////////////////////////////////////////
const int IBUFSZ = 1024;
char  inbuf[IBUFSZ] = "\0";
char *endp = inbuf;
char *readp = inbuf;
char *startp = inbuf;
int bufneeded = 0;              // The largest print buffer needed
bool prompt = false;            // True if a prompt is pending
const char EOFCHAR = '\x4';     // Ctrl-D = EOF from terminal

const int LWM  = 28000;    // Listener display buffer low water mark
const int HWM  = 30000;    // Listener display buffer high water mark

// Listener output buffer size
// This must be less than 0x7FFF - LWM to ensure that we can
// always write it to the Listener display buffer
const  int PBUFSZ = 400;

// Cue up or decue a prompt for output
void tprompt(bool active)
{
  prompt = active;
  if(!prompt)
  {
    startp = readp;
    return;
  }
  // Loop until non-space input found
  while(true)
  {
    // Skip spaces
    while((*readp != '\0') && isspace(*readp)) readp++;
    // At eoln get another line...
    if(*readp == '\0')
      getline();
    else
      break;  // Found real input
  }
}

// Get a line, prompting if required
void getline()
{
  if(prompt)
  {
    OutputString("\r\n");
    if(DebugLevel != 0)
      twritef("%d", DebugLevel);
    OutputString("> ");

    // Echo any unprocessed input
    if(Prefix)
      free(Prefix);
    Prefix = '\0';
    while(isspace(*startp)) startp++;
    if(*startp)
    {
      int len = strlen(startp);
      // Ensure a space (newline is whitespace)
      if(startp[len - 1] != ' ')
      {
        startp[len++] = ' ';
        startp[len] = '\0';
      }

      Prefix = strdup(startp);     // Save for History
      OutputString(Prefix);        // Print any unprocessed input
    }
  }
  prompt = false;
  startp = readp = endp = inbuf;

  tflushout();

  // shared_linebuf passes parameter to VCL thread
  shared_linebuf = inbuf;
  ListenerGetLine();

  if(ThreadWaitForLineAvailable())
    LispError(FATAL_EXIT);

  endp = inbuf + strlen(inbuf);
  return;
}

// Read the rest of the current line from the line buffer
char *treadln()
{
  if(readp == endp)
    getline();

  // Return the line in the buffer
  char *line = readp;
  readp = endp;
  return line;
}

// Get one character of input
int tgetch()
{
  static bool eoln = false;

  if(eoln)
  {
    eoln = false;
    getline();
  }

  if(readp == endp)
  {
    eoln = true;
    return ' ';   // Whitespace at end of line
  }

  if(*readp == CTRLD)
    return EOF;
  else return(*readp++);
}

// Unget one character of input
// We never unget whitespace and since \n is whitespace then we (should!)
// never unget at the start of a line. This means that the awkward issue
// of backing up into an already discarded line is avoided and an error
// message may be issued because something is clearly amiss.
void tungetc(int c)
{
  if((c == EOF) || (c == '\n'))
    return;
  if(c)
  {
    if(readp > inbuf)
      *--readp = (char)c;
    else
      LispError(ERR_KBD_BUF);      // Should never happen!
  }
}

// Flush the input buffer
void tflushin()
{
  startp = readp = endp = inbuf;
  inbuf[0] = '\0';
}

// Write a character to the terminal
void twritec(char c)
{
  tfwritec(stdout, c);
}

// Write a character to a stream
void tfwritec(FILE *, char c)
{
  if(c == '\n')
    OutputChar('\r');
  OutputChar(c);
}

// Write a string to a stream
void tfwrites(FILE *, char *s, int len /* = -1 */)
{
  twrites(s, len);
}

// Write a string to the terminal
void twrites(char *s, int len /* = -1 */)
{
  if(len == -1)
    len = strlen(s);

  // Copy the text, inserting \r before each \n in buffered chunks
  while(len >= 0)
  {
    char *d = fmtbuf;
    while(len-- && (d < (FBUFEND - 2)))
    {
      if(*s == '\n')
        *d++ = '\r';
      *d++ = *s++;
    }
    *d = '\0';
    OutputString(fmtbuf);
  }
}

// Write a string verbatim to the terminal
// Used by RunCommand.cpp to print command output
void twritesvb(char *s, int len /* = -1 */)
{
  if(len == -1)
    len = strlen(s);

  // Copy the text in buffered chunks
  while(len > 0)
  {
    char *d = fmtbuf;
    while(len && (d < (FBUFEND - 1)))
    {
      *d++ = *s++;
      len--;
    }

    *d = '\0';
    OutputString(fmtbuf);
  }
}

// Write formatted text to the terminal
void twritef(const char *format, ...)
{
  va_list argptr;
  va_start(argptr, format);

  (void)tvwritef(format, argptr);

  va_end(argptr);
}

// Write formatted text to a stream
void tfwritef(FILE *, const char *format, ...)
{
  va_list argptr;
  va_start(argptr, format);

  (void)tvwritef(format, argptr);

  va_end(argptr);
}

// arg vector form of twritef
void tvfwritef(FILE *, const char *format, va_list arglist)
{
  tvwritef(format, arglist);
}

// arg vector form of twritef
void tvwritef(const char *format, va_list arglist)
{
  int bn = vsnprintf(fmtbuf, FBUFSZ - 2, format, arglist);
  if(bn >= FBUFSZ-2)
    LispError(ERR_PRT_BUF);
  bufneeded = bn;

  char *d = (char*)FBUFEND;
  char *s = fmtbuf + bn;

  // Copy string backwards to end of fmtbuf expanding \n to \r\n as we go
  while((s >= fmtbuf) && (d >= s))
  {
    if((*d-- = *s--) == '\n')
      *d-- = '\r';
  }
  if(s == (fmtbuf - 1))
    OutputString(d + 1);
  else
    // Ran out of room
    LispError(ERR_PRT_BUF);
}

/////////////////////////////////////////////////////////////////////////////
// The lowest level terminal output routines, buffering etc.
/////////////////////////////////////////////////////////////////////////////
static char  prtbuf[PBUFSZ] = "";
char * const PBUFEND = &prtbuf[PBUFSZ - 2];
static char *outp = prtbuf;

// Flush prtbuf
void tflushout()
{
  if(outp == prtbuf)
    return;
  *outp = '\0';
  BufferedOutputString(prtbuf);
  outp = prtbuf;
}

// Buffer one character
static void OutputChar(char c)
{
  if(!c)
    return;
  // Need to make room?
  if((PBUFEND - outp) < 2)
    tflushout();
  *outp++ = c;
  // Flush at end of line
  if(c == '\n')
    tflushout();
}

// Buffer a string
static void OutputString(char *c)
{
  int len = strlen(c);
  if(!len)
    return;
  if((PBUFEND - outp) < len)
  {
    //No room
    tflushout();
    // Don't bother buffering big strings after flushing
    if(len > (PBUFSZ / 2))
    {
      BufferedOutputString(c);
      return;
    }
  }
  char* mark = strcpy(outp, c);
  outp += len;
  if(strchr(mark, '\n'))
    tflushout();
}

// Using messages to write to the listener window is the most visually
// attractive method and it gets around the thread communication issue
static void BufferedOutputString(char* c)
{
  // Get text size including new text to be added
  int charCount = SendMessage(ListenerHandle, WM_GETTEXTLENGTH, 0, 0) +
                  strlen(c);
  // Are we going to be above the high water mark?
  if (charCount > HWM)
  {
    // Yes, get the first line after the low water mark, measured from
    // from the end of the buffer
    int dellines = SendMessage(ListenerHandle, EM_LINEFROMCHAR,
                               charCount - LWM, 0) + 1;
    // Get the start of that line
    int delchar = SendMessage(ListenerHandle, EM_LINEINDEX, dellines, 0);
    // This shouldn't happen for reasonable output but lets be paranoid!
    if(delchar == -1) delchar = LAST;
    // Replace all prior lines with nothing
    SendMessage(ListenerHandle, EM_SETSEL, 0, delchar);
    SendMessage(ListenerHandle, EM_REPLACESEL, FALSE, (LPARAM)"");
  }
  // Goto end
  SendMessage(ListenerHandle, EM_SETSEL, LAST, LAST);
  // Insert new text
  SendMessage(ListenerHandle, EM_REPLACESEL, FALSE, (LPARAM)c);
  // Goto end
  SendMessage(ListenerHandle, EM_SETSEL, LAST, LAST);
}


