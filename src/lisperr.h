// Generated file - DO NOT EDIT
// Edit errors.lsp to make changes
#ifndef LisperrH
#define LisperrH

enum LERROR {
// Fatal errors
   FATAL_EXIT       =   0,
   FATAL_HEAP       =  -1,
   FATAL_STACK      =  -2,
   FATAL_HEAP_OVF   =  -3,
   FATAL_STACK_OVF  =  -4,
   FATAL_BAD_STACK  =  -5,
   FATAL_MEMORY     =  -6,
// Interpreter errors
   ERR_PROG         =   1,
   ERR_USER         =   2,
   ERR_NUM_ARGS     =   3,
   ERR_LIST_EXP     =   4,
   ERR_NUM_EXP      =   5,
   ERR_INT_EXP      =   6,
   ERR_REAL_EXP     =   7,
   ERR_SYM_EXP      =   8,
   ERR_KEY_EXP      =   9,
   ERR_CHR_EXP      =  10,
   ERR_STR_EXP      =  11,
   ERR_FILE_EXP     =  12,
   ERR_ATOM_EXP     =  13,
   ERR_VEC_EXP      =  14,
   ERR_INV_KEY      =  15,
   ERR_BAD_FUNC     =  16,
   ERR_BAD_MAC      =  17,
   ERR_BAD_FORMAL   =  18,
   ERR_IMMUTABLE    =  19,
   ERR_BAD_ASSOC    =  20,
   ERR_BAD_PLIST    =  21,
   ERR_NULL_LOOP    =  22,
   ERR_NUM_FORMAL   =  23,
   ERR_BAD_REAL     =  24,
   ERR_BAD_INT      =  25,
   ERR_BAD_ARG      =  26,
   ERR_LET_BIND     =  27,
   ERR_BAD_CASE     =  28,
   ERR_NO_CASE      =  29,
   ERR_BAD_ASCII    =  30,
   ERR_STR_LEN      =  31,
   ERR_SYM_LEN      =  32,
   ERR_BAD_TYP      =  33,
   ERR_CATCH        =  34,
   ERR_BIT_NUM      =  35,
   ERR_INDEX        =  36,
   ERR_VEC_DIM      =  37,
   ERR_HEAP         =  38,
   ERR_APPEND_DOT   =  39,
   ERR_VSORT_CMP    =  40,
// Maths errors
// Maths errors raised within the interpreter will have more
// helpful error messages which specify the function that
// failed.  These messages are provided for user.
   ERR_M_DOM        = 100,
   ERR_M_SING       = 101,
   ERR_M_OVRF       = 102,
   ERR_M_UNDF       = 103,
   ERR_M_INAC       = 104,
   ERR_M_INV        = 105,
   ERR_M_DIVZ       = 106,
   ERR_M_IOVF       = 107,
   ERR_M_UNK        = 108,
   ERR_M_FPVAL      = 109,
// I/O errors
   ERR_EOF_ESC      = 200,
   ERR_EOF_STR      = 201,
   ERR_EOF_LIST     = 202,
   ERR_STR_CHR      = 203,
   ERR_BAD_RPAR     = 204,
   ERR_NO_RPAR      = 205,
   ERR_PATOM        = 206,
   ERR_TOK_LEN      = 207,
   ERR_BAD_RMAC     = 208,
   ERR_UNREADABLE   = 209,
   ERR_PRT_BUF      = 210,
   ERR_KBD_BUF      = 211,
   ERR_DOT_SYNTAX   = 212,
   ERR_NOT_FOUND    = 213,
// Operating System errors
// Message always provided by Windows/interpreter
   ERR_OS           = 300,
   ERR_PROCESS      = 301,
// Graphics errors
   ERR_WIN_OPEN     = 400,
   ERR_WIN_EXP      = 401,
   ERR_INV_VP       = 402,
   ERR_INV_BSTYL    = 403,
   ERR_INV_PSTYL    = 404,
   ERR_VT2_SING     = 405,
// Unknown error
   ERR_UNKNOWN      = 998,
// Debug
   ERR_DEBUG        = 999,
   LERR_DUMMY };

const char* err_msg(LERROR e);
void DefineErrorConst();

#endif
