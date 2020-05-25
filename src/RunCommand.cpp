////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// RunCommand.cpp
//
// Run a command on the windows shell (cmd.exe/command.com)
////////////////////////////////////////////////////////////////////////////
//
// A much modified example nicked from somewhere on the Web.
// This is because MSDN is not exactly helpful with the details...
// Seems to work very well
 
#include <stdio.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#pragma hdrstop

#include "AJBLisp.h"

//---------------------------------------------------------------------------
#define BUFSIZE 511

HANDLE              hChildStdinRd;
HANDLE              hChildStdinWr;
HANDLE              hChildStdinWrDup;
HANDLE              hChildStdoutRd;
HANDLE              hChildStdoutWr;
HANDLE              hChildStdoutRdDup;
HANDLE              hInputFile;
HANDLE              hStdout;
PROCESS_INFORMATION pi;

void CreateChildProcess(char *cmd, char *dir);
void WriteToPipe(FILE *in);
void ReadFromPipe(FILE *out);
void ErrorExit(LPTSTR);
void ErrMsg(LPTSTR, BOOL);


unsigned long RunCommand(char* cmd, FILE* out, FILE* in, char *dir)
{ 
  SECURITY_ATTRIBUTES Attr;
  BOOL ok;

  // Set the bInheritHandle flag so pipe handles are inherited.
  Attr.nLength = sizeof(SECURITY_ATTRIBUTES);
  Attr.bInheritHandle = TRUE;
  Attr.lpSecurityDescriptor = NULL;

  // Get the handle to the current STDOUT.
  hStdout = GetStdHandle(STD_OUTPUT_HANDLE);

  // Create a pipe for the child process's STDOUT.
  if(!CreatePipe(&hChildStdoutRd, &hChildStdoutWr, &Attr, 0))
    ErrorExit("System: Stdout pipe creation failed\n");

  // Create noninheritable read handle and close the inheritable read
  // handle.
  ok = DuplicateHandle(GetCurrentProcess(),
                       hChildStdoutRd,
                       GetCurrentProcess(),
                       &hChildStdoutRdDup ,
                       0,
                       FALSE,
                       DUPLICATE_SAME_ACCESS);
  if(!ok)
    ErrorExit("DuplicateHandle failed");
  CloseHandle(hChildStdoutRd);

  // Create a pipe for the child process's STDIN.
  if(!CreatePipe(&hChildStdinRd, &hChildStdinWr, &Attr, 0))
    ErrorExit("System: Stdin pipe creation failed\n");
 
  // Duplicate the write handle to the pipe so it is not inherited.
  ok = DuplicateHandle(GetCurrentProcess(),
                       hChildStdinWr,
                       GetCurrentProcess(),
                       &hChildStdinWrDup,
                       0,
                       FALSE,                 // not inherited
                       DUPLICATE_SAME_ACCESS);
  if(!ok)
    ErrorExit("System: DuplicateHandle failed");

  CloseHandle(hChildStdinWr);

  // Now create the child process.
  CreateChildProcess(cmd, dir);

  // Write to pipe that is the standard input for a child process.
  WriteToPipe(in);

  // Read from pipe that is the standard output for child process.
  ReadFromPipe(out);

  unsigned long rc;
  ok = GetExitCodeProcess(pi.hProcess, &rc);
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);

  if(!ok)
    WindowsError();

  return rc;
}
 
void CreateChildProcess(char *cmd, char *dir)
{ 
  STARTUPINFO         si;
  BOOL                ok;

  // Set up members of the PROCESS_INFORMATION structure.
  ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));

  // Set up members of the STARTUPINFO structure.
  ZeroMemory(&si, sizeof(STARTUPINFO));
  si.cb = sizeof(STARTUPINFO);
  si.hStdError    = hChildStdoutWr;
  si.hStdOutput   = hChildStdoutWr;
  si.hStdInput    = hChildStdinRd;
  si.dwFlags     |= STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  si.wShowWindow  = SW_HIDE;

  // Create the child process.
  char shellCmd[1024];
  if(!GetEnvironmentVariable(("ComSpec"), shellCmd, _MAX_PATH))
    ErrorExit("System: Can't find command processor");

  strcat(shellCmd, " /A /C ");      // run command with ASCII output
  strcat(shellCmd, cmd);

  ok = CreateProcess(NULL,
                     shellCmd,      // command line
                     NULL,          // process security attributes
                     NULL,          // primary thread security attributes
                     TRUE,          // handles are inherited
                     0,             // creation flags
                     NULL,          // use parent's environment
                     dir,           // the current directory
                     &si,           // STARTUPINFO pointer
                     &pi);          // receives PROCESS_INFORMATION
  if(!ok)
    WindowsError();
}

void WriteToPipe(FILE* in)
{
  unsigned long read, written;
  char buf[BUFSIZE];

  if(!in) return;

  // Read from a file and write its contents to a pipe.
  do
  {
    read = fread(buf, 1, BUFSIZE, in);
  } while(WriteFile(hChildStdinWrDup, buf, read, &written, NULL));

  // Close the pipe handle so the child process stops reading.
  if(!CloseHandle(hChildStdinWrDup))
    ErrorExit("System: Close pipe failed");
}

void ReadFromPipe(FILE* out)
{
  unsigned long read;
  char buf[BUFSIZE];

  // Close the write end of the pipe before reading from the
  // read end of the pipe.
  if(!CloseHandle(hChildStdoutWr))
    ErrorExit("System: CloseHandle failed");

  // Read output from the child process, and write to parent's STDOUT.
  while(ReadFile(hChildStdoutRdDup, buf, BUFSIZE, &read, NULL) && read)
  {
    if(out)
      fwrites(out, buf, read);
    else
      twritesvb(buf, read);   // Write verbatim to listener
  }
}

void ErrorExit(char *msg)
{
  LispError(ERR_PROCESS, NONE, msg);
}

