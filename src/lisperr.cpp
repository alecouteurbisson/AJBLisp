// Generated file - DO NOT EDIT
// Edit errors.lsp to make changes
#include "ajblisp.h"

const char* err_msg(LERROR e)
{
  char* msg;
  switch(e)
  {
// Fatal errors
    case  FATAL_EXIT       : msg = ""; break;
    case  FATAL_HEAP       : msg = "Failed to allocate heap"; break;
    case  FATAL_STACK      : msg = "Failed to allocate stack"; break;
    case  FATAL_HEAP_OVF   : msg = "Out of heap space"; break;
    case  FATAL_STACK_OVF  : msg = "Out of stack space"; break;
    case  FATAL_BAD_STACK  : msg = "Corrupted lisp stack"; break;
    case  FATAL_MEMORY     : msg = "Out of system memory"; break;
// Interpreter errors
    case  ERR_PROG         : msg = "Programmed error"; break;
    case  ERR_USER         : msg = "User interrupt"; break;
    case  ERR_NUM_ARGS     : msg = "Incorrect number of arguments to function"; break;
    case  ERR_LIST_EXP     : msg = "List argument expected"; break;
    case  ERR_NUM_EXP      : msg = "Numeric argument expected"; break;
    case  ERR_INT_EXP      : msg = "Integer argument expected"; break;
    case  ERR_REAL_EXP     : msg = "Real argument expected"; break;
    case  ERR_SYM_EXP      : msg = "Symbol argument expected"; break;
    case  ERR_KEY_EXP      : msg = "Key argument expected"; break;
    case  ERR_CHR_EXP      : msg = "Character argument expected"; break;
    case  ERR_STR_EXP      : msg = "String argument expected"; break;
    case  ERR_FILE_EXP     : msg = "File argument expected"; break;
    case  ERR_ATOM_EXP     : msg = "Atomic argument expected"; break;
    case  ERR_VEC_EXP      : msg = "Vector argument expected"; break;
    case  ERR_INV_KEY      : msg = "Invalid key argument"; break;
    case  ERR_BAD_FUNC     : msg = "Illegal function"; break;
    case  ERR_BAD_MAC      : msg = "Illegal macro"; break;
    case  ERR_BAD_FORMAL   : msg = "Illegal formal argument"; break;
    case  ERR_IMMUTABLE    : msg = "Cannot change value of a constant"; break;
    case  ERR_BAD_ASSOC    : msg = "Badly formed association list"; break;
    case  ERR_BAD_PLIST    : msg = "Badly formed property list"; break;
    case  ERR_NULL_LOOP    : msg = "Aborted null loop"; break;
    case  ERR_NUM_FORMAL   : msg = "Too many arguments for stack frame"; break;
    case  ERR_BAD_REAL     : msg = "Cannot interpret string as real"; break;
    case  ERR_BAD_INT      : msg = "Cannot interpret string as integer"; break;
    case  ERR_BAD_ARG      : msg = "Invalid argument(s)"; break;
    case  ERR_LET_BIND     : msg = "Badly formed binding in LET"; break;
    case  ERR_BAD_CASE     : msg = "Badly formed case expression"; break;
    case  ERR_NO_CASE      : msg = "No matching case found"; break;
    case  ERR_BAD_ASCII    : msg = "Illegal ASCII value"; break;
    case  ERR_STR_LEN      : msg = "String buffer overflow"; break;
    case  ERR_SYM_LEN      : msg = "Symbol name too long (>240 characters)"; break;
    case  ERR_BAD_TYP      : msg = "Unrecognised argument type"; break;
    case  ERR_CATCH        : msg = "Throw not caught"; break;
    case  ERR_BIT_NUM      : msg = "Illegal bit number"; break;
    case  ERR_INDEX        : msg = "Vector/List/String index out of range"; break;
    case  ERR_VEC_DIM      : msg = "Vector dimension must be a whole number"; break;
    case  ERR_HEAP         : msg = "Heap space very low"; break;
    case  ERR_APPEND_DOT   : msg = "Attempt to append onto dotted list"; break;
    case  ERR_VSORT_CMP    : msg = "Non-integer compare result in vsort"; break;
// Maths errors
// Maths errors raised within the interpreter will have more
// helpful error messages which specify the function that
// failed.  These messages are provided for user.
    case  ERR_M_DOM        : msg = "Maths - domain error"; break;
    case  ERR_M_SING       : msg = "Maths - result singular"; break;
    case  ERR_M_OVRF       : msg = "Maths - result overflow"; break;
    case  ERR_M_UNDF       : msg = "Maths - result underflow"; break;
    case  ERR_M_INAC       : msg = "Maths - result inaccurate"; break;
    case  ERR_M_INV        : msg = "Maths - invalid operation"; break;
    case  ERR_M_DIVZ       : msg = "Maths - divide by zero"; break;
    case  ERR_M_IOVF       : msg = "Maths - integer overflow"; break;
    case  ERR_M_UNK        : msg = "Maths - unknown error"; break;
    case  ERR_M_FPVAL      : msg = "Maths - illegal ieee float value"; break;
// I/O errors
    case  ERR_EOF_ESC      : msg = "EOF after reading escape character"; break;
    case  ERR_EOF_STR      : msg = "EOF inside string"; break;
    case  ERR_EOF_LIST     : msg = "EOF inside list"; break;
    case  ERR_STR_CHR      : msg = "Illegal character inside string"; break;
    case  ERR_BAD_RPAR     : msg = "Unexpected close parenthesis"; break;
    case  ERR_NO_RPAR      : msg = "Missing close parenthesis"; break;
    case  ERR_PATOM        : msg = "List argument to patom"; break;
    case  ERR_TOK_LEN      : msg = "Input token exceeds buffer size"; break;
    case  ERR_BAD_RMAC     : msg = "Illegal input after '#'"; break;
    case  ERR_UNREADABLE   : msg = "Unreadable input; #<..."; break;
    case  ERR_PRT_BUF      : msg = "Output buffer overflow"; break;
    case  ERR_KBD_BUF      : msg = "Keyboard buffer underflow"; break;
    case  ERR_DOT_SYNTAX   : msg = "Invalid dotted pair syntax"; break;
    case  ERR_NOT_FOUND    : msg = "File not found"; break;
// Operating System errors
// Message always provided by Windows/interpreter
    case  ERR_OS           : msg = "OS Error"; break;
    case  ERR_PROCESS      : msg = "Shell process error"; break;
// Graphics errors
    case  ERR_WIN_OPEN     : msg = "GFX - Failed to open window"; break;
    case  ERR_WIN_EXP      : msg = "GFX - Window argument expected"; break;
    case  ERR_INV_VP       : msg = "GFX - Invalid Viewport"; break;
    case  ERR_INV_BSTYL    : msg = "GFX - Invalid brush style"; break;
    case  ERR_INV_PSTYL    : msg = "GFX - Invalid pen style"; break;
    case  ERR_VT2_SING     : msg = "GFX - 2D View transform nearly singular"; break;
// Unknown error
    case  ERR_UNKNOWN      : msg = "Unknown error"; break;
// Debug
    case  ERR_DEBUG        : msg = "Debug error"; break;
    default                : msg = "Unknown error";
  }
  return msg;
}


void DefineErrorConst()
{
// Fatal errors
  NewConstant(     ":fatal_exit", NewInteger(0));
  NewConstant(     ":fatal_heap", NewInteger(-1));
  NewConstant(    ":fatal_stack", NewInteger(-2));
  NewConstant( ":fatal_heap_ovf", NewInteger(-3));
  NewConstant(":fatal_stack_ovf", NewInteger(-4));
  NewConstant(":fatal_bad_stack", NewInteger(-5));
  NewConstant(   ":fatal_memory", NewInteger(-6));
// Interpreter errors
  NewConstant(       ":err_prog", NewInteger(1));
  NewConstant(       ":err_user", NewInteger(2));
  NewConstant(   ":err_num_args", NewInteger(3));
  NewConstant(   ":err_list_exp", NewInteger(4));
  NewConstant(    ":err_num_exp", NewInteger(5));
  NewConstant(    ":err_int_exp", NewInteger(6));
  NewConstant(   ":err_real_exp", NewInteger(7));
  NewConstant(    ":err_sym_exp", NewInteger(8));
  NewConstant(    ":err_key_exp", NewInteger(9));
  NewConstant(    ":err_chr_exp", NewInteger(10));
  NewConstant(    ":err_str_exp", NewInteger(11));
  NewConstant(   ":err_file_exp", NewInteger(12));
  NewConstant(   ":err_atom_exp", NewInteger(13));
  NewConstant(    ":err_vec_exp", NewInteger(14));
  NewConstant(    ":err_inv_key", NewInteger(15));
  NewConstant(   ":err_bad_func", NewInteger(16));
  NewConstant(    ":err_bad_mac", NewInteger(17));
  NewConstant( ":err_bad_formal", NewInteger(18));
  NewConstant(  ":err_immutable", NewInteger(19));
  NewConstant(  ":err_bad_assoc", NewInteger(20));
  NewConstant(  ":err_bad_plist", NewInteger(21));
  NewConstant(  ":err_null_loop", NewInteger(22));
  NewConstant( ":err_num_formal", NewInteger(23));
  NewConstant(   ":err_bad_real", NewInteger(24));
  NewConstant(    ":err_bad_int", NewInteger(25));
  NewConstant(    ":err_bad_arg", NewInteger(26));
  NewConstant(   ":err_let_bind", NewInteger(27));
  NewConstant(   ":err_bad_case", NewInteger(28));
  NewConstant(    ":err_no_case", NewInteger(29));
  NewConstant(  ":err_bad_ascii", NewInteger(30));
  NewConstant(    ":err_str_len", NewInteger(31));
  NewConstant(    ":err_sym_len", NewInteger(32));
  NewConstant(    ":err_bad_typ", NewInteger(33));
  NewConstant(      ":err_catch", NewInteger(34));
  NewConstant(    ":err_bit_num", NewInteger(35));
  NewConstant(      ":err_index", NewInteger(36));
  NewConstant(    ":err_vec_dim", NewInteger(37));
  NewConstant(       ":err_heap", NewInteger(38));
  NewConstant( ":err_append_dot", NewInteger(39));
  NewConstant(  ":err_vsort_cmp", NewInteger(40));
// Maths errors
// Maths errors raised within the interpreter will have more
// helpful error messages which specify the function that
// failed.  These messages are provided for user.
  NewConstant(      ":err_m_dom", NewInteger(100));
  NewConstant(     ":err_m_sing", NewInteger(101));
  NewConstant(     ":err_m_ovrf", NewInteger(102));
  NewConstant(     ":err_m_undf", NewInteger(103));
  NewConstant(     ":err_m_inac", NewInteger(104));
  NewConstant(      ":err_m_inv", NewInteger(105));
  NewConstant(     ":err_m_divz", NewInteger(106));
  NewConstant(     ":err_m_iovf", NewInteger(107));
  NewConstant(      ":err_m_unk", NewInteger(108));
  NewConstant(    ":err_m_fpval", NewInteger(109));
// I/O errors
  NewConstant(    ":err_eof_esc", NewInteger(200));
  NewConstant(    ":err_eof_str", NewInteger(201));
  NewConstant(   ":err_eof_list", NewInteger(202));
  NewConstant(    ":err_str_chr", NewInteger(203));
  NewConstant(   ":err_bad_rpar", NewInteger(204));
  NewConstant(    ":err_no_rpar", NewInteger(205));
  NewConstant(      ":err_patom", NewInteger(206));
  NewConstant(    ":err_tok_len", NewInteger(207));
  NewConstant(   ":err_bad_rmac", NewInteger(208));
  NewConstant( ":err_unreadable", NewInteger(209));
  NewConstant(    ":err_prt_buf", NewInteger(210));
  NewConstant(    ":err_kbd_buf", NewInteger(211));
  NewConstant( ":err_dot_syntax", NewInteger(212));
  NewConstant(  ":err_not_found", NewInteger(213));
// Operating System errors
// Message always provided by Windows/interpreter
  NewConstant(         ":err_os", NewInteger(300));
  NewConstant(    ":err_process", NewInteger(301));
// Graphics errors
  NewConstant(   ":err_win_open", NewInteger(400));
  NewConstant(    ":err_win_exp", NewInteger(401));
  NewConstant(     ":err_inv_vp", NewInteger(402));
  NewConstant(  ":err_inv_bstyl", NewInteger(403));
  NewConstant(  ":err_inv_pstyl", NewInteger(404));
  NewConstant(   ":err_vt2_sing", NewInteger(405));
// Unknown error
  NewConstant(    ":err_unknown", NewInteger(998));
// Debug
  NewConstant(      ":err_debug", NewInteger(999));
}
