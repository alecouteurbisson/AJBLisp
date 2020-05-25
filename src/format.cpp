// Formatted output
// (format "%s, %d : %#02x" "name" 5 10) -> "name, 5 : 0x0a"

// Print spaces
// If n <= 0 do nothing
// Else print n spaces.
void Spaces(FILE* f, int n, int w)
{
  const int SWIDTH = 20
  static const char* s = "                    "; // SWIDTH spaces
  if(n <= 0)
    return;
  while((n -= SWIDTH) > 0)
    fputs(s, f);
  fputs(s - n, f);
}


void iFormat(FILE* f, char* fmt, LISP args)
{
  char  buf[80];
  char  txt[20];

  bool  rjust = false;
  int   width = 0;
  char  spec;

  int needed;
  int pad;

  int pc = fmt - 1;
  int oldpc = fmt;
  while (pc = strchr(pc + 1, '%'))
  {
    if(!pc)
    {
      fputs(oldpc, f);
      break;
    }

    fwrite(oldpc, pc - oldpc, 1, f);
    oldpc = pc;

    // %% translates to %
    if(pc[1] == '%')
    {
      fputc('%', f);
      oldpc = (pc += 2);
      continue;
    }

    if(!args)
      LispError(ERR_NUM_ARGS, NONE, "Missing argument for format placeholder");
    LISP arg = car(args);
    args = cdr(args);

    // read flag characters
    bool flag = true;
    while(flag)
    {
      switch(*pc)
      {
        case '-': rjust = true;
        case '+':
        case '#':
        case ' ':
        case '0': pc++; break;
        default: flag = false;
      }
    }
    // read width
    if(isdigit(*pc)
    {
      width = 0;
      while(isdigit(*pc))
      {
        width *= 10;
        width += (*pc++ - '0');
      }
    }

    // skip .
    if(*pc == '.')
    {
      pc++;

      // skip precision
      while(isdigit(*pc++))
    }

    spec = *pc++;

    int len = pc - oldpc;
    oldpc = pc;
    if(len > 18)
      LispError(ERR_PRT_BUF, NONE, "Format placeholder buffer overflow")

    // Copy format inserting 'l' for a long/double argument
    memcpy(txt, oldpc, len);
    txt[len - 1] = 'l';
    txt[len] = spec;
    txt[len + 1] = '\0';

    switch(spec)
    {
      case 'd':
      case 'x':
        EXPECT_INT(arg);
        fprintf(f, txt, ivalue(arg));
      break;

      case 'f':
      case 'e':
        EXPECT_FLT(arg);
        fprintf(f, txt, rvalue(arg));
      break;

      case 'a':
        pad = width - iPrintSize(arg, true);
        if(rjust && (pad > 0))
          Spaces(f, pad);
        Print(f, arg, true);
        if(!rjust && (pad > 0))
          Spaces(f, pad);
      break;

      case 'w':
        pad = width - iPrintSize(arg, false);
        if(rjust && (pad > 0))
          Spaces(f, pad);
        Print(f, arg, false);
        if(!rjust && (pad > 0))
          Spaces(f, pad);
      break;

      case 's':
        needed = strlen(text(arg));
        if(needed > width) needed = width;
        pad = width - needed;
        if(rjust && (pad > 0))
          Spaces(f, pad);
        fwrite(text(arg), needed, 1, f);
        if(!rjust && (pad > 0))
          Spaces(f, pad);
      break;

      default:
        LispError(ERR_BAD_ARG, NewChar(spec), "Bad format placeholder")
    }
  }
}

