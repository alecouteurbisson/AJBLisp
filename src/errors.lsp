;;seq   symbol           message
(
"Fatal errors"
( 0   :fatal_exit      "")
( -   :fatal_heap      "Failed to allocate heap")
( -   :fatal_stack     "Failed to allocate stack")
( -   :fatal_heap_ovf  "Out of heap space")
( -   :fatal_stack_ovf "Out of stack space")
( -   :fatal_bad_stack "Corrupted lisp stack")
( -   :fatal_memory    "Out of system memory")

"Interpreter errors"
( 1   :err_prog        "Programmed error")
( +   :err_user        "User interrupt")
( +   :err_num_args    "Incorrect number of arguments to function")
( +   :err_list_exp    "List argument expected")
( +   :err_num_exp     "Numeric argument expected")
( +   :err_int_exp     "Integer argument expected")
( +   :err_real_exp    "Real argument expected")
( +   :err_sym_exp     "Symbol argument expected")
( +   :err_key_exp     "Key argument expected")
( +   :err_chr_exp     "Character argument expected")
( +   :err_str_exp     "String argument expected")
( +   :err_file_exp    "File argument expected")
( +   :err_atom_exp    "Atomic argument expected")
( +   :err_vec_exp     "Vector argument expected")
( +   :err_inv_key     "Invalid key argument")
( +   :err_bad_func    "Illegal function")
( +   :err_bad_mac     "Illegal macro")
( +   :err_bad_formal  "Illegal formal argument")
( +   :err_immutable   "Cannot change value of a constant")
( +   :err_bad_assoc   "Badly formed association list")
( +   :err_bad_plist   "Badly formed property list")
( +   :err_null_loop   "Aborted null loop")
( +   :err_num_formal  "Too many arguments for stack frame")
( +   :err_bad_real    "Cannot interpret string as real")
( +   :err_bad_int     "Cannot interpret string as integer")
( +   :err_bad_arg     "Invalid argument(s)")
( +   :err_let_bind    "Badly formed binding in LET")
( +   :err_bad_case    "Badly formed case expression")
( +   :err_no_case     "No matching case found")
( +   :err_bad_ascii   "Illegal ASCII value")
( +   :err_str_len     "String buffer overflow")
( +   :err_sym_len     "Symbol name too long (>240 characters)")
( +   :err_bad_typ     "Unrecognised argument type")
( +   :err_catch       "Throw not caught")
( +   :err_bit_num     "Illegal bit number")
( +   :err_index       "Vector/List/String index out of range")
( +   :err_vec_dim     "Vector dimension must be a whole number")
( +   :err_heap        "Heap space very low")
( +   :err_append_dot  "Attempt to append onto dotted list")
( +   :err_vsort_cmp   "Non-integer compare result in vsort")

"Maths errors"
"Maths errors raised within the interpreter will have more"
"helpful error messages which specify the function that"
"failed.  These messages are provided for user."
(100  :err_m_dom       "Maths - domain error")
( +   :err_m_sing      "Maths - result singular")
( +   :err_m_ovrf      "Maths - result overflow")
( +   :err_m_undf      "Maths - result underflow")
( +   :err_m_inac      "Maths - result inaccurate")
( +   :err_m_inv       "Maths - invalid operation")
( +   :err_m_divz      "Maths - divide by zero")
( +   :err_m_iovf      "Maths - integer overflow")
( +   :err_m_unk       "Maths - unknown error")
( +   :err_m_fpval     "Maths - illegal ieee float value")

"I/O errors"
(200  :err_eof_esc     "EOF after reading escape character")
( +   :err_eof_str     "EOF inside string")
( +   :err_eof_list    "EOF inside list")
( +   :err_str_chr     "Illegal character inside string")
( +   :err_bad_rpar    "Unexpected close parenthesis")
( +   :err_no_rpar     "Missing close parenthesis")
( +   :err_patom       "List argument to patom")
( +   :err_tok_len     "Input token exceeds buffer size")
( +   :err_bad_rmac    "Illegal input after '#'")
( +   :err_unreadable  "Unreadable input; #<...")
( +   :err_prt_buf     "Output buffer overflow")
( +   :err_kbd_buf     "Keyboard buffer underflow")
( +   :err_dot_syntax  "Invalid dotted pair syntax")
( +   :err_not_found   "File not found")

"Operating System errors"
"Message always provided by Windows/interpreter"
(300  :err_os          "OS Error")
( +   :err_process     "Shell process error")

"Graphics errors"
(400  :err_win_open    "GFX - Failed to open window")
( +   :err_win_exp     "GFX - Window argument expected")
( +   :err_inv_vp      "GFX - Invalid Viewport")
( +   :err_inv_bstyl   "GFX - Invalid brush style")
( +   :err_inv_pstyl   "GFX - Invalid pen style")
( +   :err_vt2_sing    "GFX - 2D View transform nearly singular")

"Unknown error"
(998  :err_unknown     "Unknown error")

"Debug"
(999  :err_debug       "Debug error")
)
