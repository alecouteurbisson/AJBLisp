	.686p
	ifdef ??version
	if    ??version GT 500H
	.mmx
	endif
	endif
	model flat
	ifndef	??version
	?debug	macro
	endm
	endif
	?debug	S "D:\My Documents\Prog\CPP\AJBLisp\V3.7\src\FuncAsm.cpp"
	?debug	T "D:\My Documents\Prog\CPP\AJBLisp\V3.7\src\FuncAsm.cpp"
_TEXT	segment dword public use32 'CODE'
_TEXT	ends
_DATA	segment dword public use32 'DATA'
_DATA	ends
_BSS	segment dword public use32 'BSS'
_BSS	ends
DGROUP	group	_BSS,_DATA
_TEXT	segment dword public use32 'CODE'
@Add$qqrrpw4CELL	segment virtual
	align	2
@@Add$qqrrpw4CELL	proc	near
?live16385@0:
	push      esi
	add       esp,-12
?live16385@16: ; EAX = args
@1:
	xor       esi,esi
?live16385@32: ; ESI = result, EAX = args
	xor       edx,edx
	mov       dword ptr [esp],edx
	mov       dword ptr [esp+4],edx
	mov       cl,1
?live16385@64: ; ECX = intf, ESI = result, EAX = args
	mov       eax,dword ptr [eax]
?live16385@80: ; EAX = a, ECX = intf, ESI = result
@2:
	test      eax,eax
	je        @4
@3:
	mov       edx,dword ptr [eax+2]
	test      edx,edx
	je        short @8
	cmp       byte ptr [edx],6
	je        short @6
	cmp       byte ptr [edx],7
	je        short @6
@8:
	xor       edx,edx
	jmp       short @7
@6:
	mov       edx,1
@7:
	test      dl,dl
	jne       short @5
	xor       ecx,ecx
	mov       edx,dword ptr [eax+2]
	mov       ax,5
	call      @@LispError$qqr6LERRORpw4CELLpc
	jmp       @9
@5:
	test      cl,cl
	je        short @10
	mov       edx,dword ptr [eax+2]
	test      edx,edx
	je        short @14
	cmp       byte ptr [edx],7
	je        short @12
@14:
	xor       edx,edx
	jmp       short @13
@12:
	mov       edx,1
@13:
	test      dl,dl
	je        short @11
?live16385@144: ; EAX = a, ESI = result
	xor       ecx,ecx
?live16385@160: ; EAX = a, ECX = intf, ESI = result
	mov       dword ptr [esp+8],esi
	fild      dword ptr [esp+8]
	fstp      qword ptr [esp]
	jmp       short @2
@11:
	mov       edx,dword ptr [eax+2]
	add       esi,dword ptr [edx+2]
?live16385@208: ; 
 	into	
?live16385@224: ; EAX = a, ECX = intf, ESI = result
	jmp       short @15
@10:
	mov       edx,dword ptr [eax+2]
	test      edx,edx
	je        short @19
	cmp       byte ptr [edx],7
	je        short @17
@19:
	xor       edx,edx
	jmp       short @18
@17:
	mov       edx,1
@18:
	test      dl,dl
	je        short @16
	mov       edx,dword ptr [eax+2]
	fld       qword ptr [edx+2]
	fadd      qword ptr [esp]
	fstp      qword ptr [esp]
	jmp       short @20
@16:
	mov       edx,dword ptr [eax+2]
	fild      dword ptr [edx+2]
	fadd      qword ptr [esp]
	fstp      qword ptr [esp]
@20:
@15:
	mov       eax,dword ptr [eax+6]
	test      eax,eax
	jne       @3
?live16385@320: ; ECX = intf, ESI = result
@4:
	test      cl,cl
	je        short @21
?live16385@336: ; ESI = result
	mov       eax,esi
	call      @@NewInteger$qqri
	jmp       short @9
?live16385@352: ; 
@21:
	push      dword ptr [esp+4]
	push      dword ptr [esp+4]
	call      @@NewReal$qqrd
@22:
@23:
@9:
	add       esp,12
	pop       esi
	ret 
@@Add$qqrrpw4CELL	endp
@Add$qqrrpw4CELL	ends
_TEXT	ends
_TEXT	segment dword public use32 'CODE'
@Subtract$qqrucppw4CELL	segment virtual
	align	2
@@Subtract$qqrucppw4CELL	proc	near
?live16386@0:
@24:
	cmp       al,1
	jne       short @26
?live16386@16: ; EDX = args
@25:
	mov       eax,dword ptr [edx]
	test      eax,eax
	je        short @30
	cmp       byte ptr [eax],6
	je        short @28
	cmp       byte ptr [eax],7
	je        short @28
@30:
	xor       eax,eax
	jmp       short @29
@28:
	mov       eax,1
@29:
	test      al,al
	jne       short @27
	mov       ax,5
	xor       ecx,ecx
	mov       edx,dword ptr [edx]
	call      @@LispError$qqr6LERRORpw4CELLpc
@64:
	ret 
@27:
	mov       eax,dword ptr [edx]
	test      eax,eax
	je        short @35
	cmp       byte ptr [eax],6
	je        short @33
@35:
	xor       ecx,ecx
	jmp       short @34
@33:
	mov       ecx,1
@34:
	test      cl,cl
	je        short @32
	mov       eax,dword ptr [edx]
	mov       eax,dword ptr [eax+2]
	neg       eax
	call      @@NewInteger$qqri
@65:
	ret 
@32:
	mov       edx,dword ptr [edx]
	add       esp,-8
	fld       qword ptr [edx+2]
	fchs
	fstp      qword ptr [esp]
	call      @@NewReal$qqrd
@66:
	ret 
?live16386@80: ; EDX = args, EAX = nargs
@36:
@26:
	cmp       al,2
	jne       @38
?live16386@96: ; EDX = args
@37:
	mov       eax,dword ptr [edx]
	test      eax,eax
	je        short @42
	cmp       byte ptr [eax],6
	je        short @40
	cmp       byte ptr [eax],7
	je        short @40
@42:
	xor       ecx,ecx
	jmp       short @41
@40:
	mov       ecx,1
@41:
	test      cl,cl
	jne       short @39
	mov       ax,5
	xor       ecx,ecx
	mov       edx,dword ptr [edx]
	call      @@LispError$qqr6LERRORpw4CELLpc
@67:
	ret 
@39:
	mov       eax,dword ptr [edx+4]
	test      eax,eax
	je        short @46
	cmp       byte ptr [eax],6
	je        short @44
	cmp       byte ptr [eax],7
	je        short @44
@46:
	xor       eax,eax
	jmp       short @45
@44:
	mov       eax,1
@45:
	test      al,al
	jne       short @43
	mov       ax,5
	xor       ecx,ecx
	mov       edx,dword ptr [edx+4]
	call      @@LispError$qqr6LERRORpw4CELLpc
@68:
	ret 
@43:
	mov       eax,dword ptr [edx]
	test      eax,eax
	je        short @50
	cmp       byte ptr [eax],7
	je        short @48
@50:
	xor       ecx,ecx
	jmp       short @49
@48:
	mov       ecx,1
@49:
	test      cl,cl
	je        short @47
	mov       eax,dword ptr [edx+4]
	test      eax,eax
	je        short @54
	cmp       byte ptr [eax],7
	je        short @52
@54:
	xor       eax,eax
	jmp       short @53
@52:
	mov       eax,1
@53:
	test      al,al
	je        short @51
	mov       ecx,dword ptr [edx]
	fld       qword ptr [ecx+2]
	mov       edx,dword ptr [edx+4]
	fsub      qword ptr [edx+2]
	add       esp,-8
	fstp      qword ptr [esp]
	call      @@NewReal$qqrd
@69:
	ret 
@51:
	mov       eax,dword ptr [edx+4]
	fild      dword ptr [eax+2]
	mov       edx,dword ptr [edx]
	fsubr     qword ptr [edx+2]
	add       esp,-8
	fstp      qword ptr [esp]
	call      @@NewReal$qqrd
@70:
	ret 
@47:
	mov       eax,dword ptr [edx+4]
	test      eax,eax
	je        short @58
	cmp       byte ptr [eax],7
	je        short @56
@58:
	xor       ecx,ecx
	jmp       short @57
@56:
	mov       ecx,1
@57:
	test      cl,cl
	je        short @55
	mov       eax,dword ptr [edx]
	fild      dword ptr [eax+2]
	mov       edx,dword ptr [edx+4]
	fsub      qword ptr [edx+2]
	add       esp,-8
	fstp      qword ptr [esp]
	call      @@NewReal$qqrd
@71:
	ret 
@55:
@59:
	mov       ecx,dword ptr [edx]
	mov       eax,dword ptr [ecx+2]
	mov       edx,dword ptr [edx+4]
	sub       eax,dword ptr [edx+2]
?live16386@224: ; 
 	into	
?live16386@240: ; EAX = i
	call      @@NewInteger$qqri
@72:
	ret 
?live16386@256: ; 
@60:
@61:
@62:
@38:
	xor       ecx,ecx
	mov       edx,dword ptr [_NONE]
	mov       ax,3
	call      @@LispError$qqr6LERRORpw4CELLpc
	xor       eax,eax
@63:
@31:
	ret 
@@Subtract$qqrucppw4CELL	endp
@Subtract$qqrucppw4CELL	ends
_TEXT	ends
_TEXT	segment dword public use32 'CODE'
@Multiply$qqrrpw4CELL	segment virtual
	align	2
@@Multiply$qqrrpw4CELL	proc	near
?live16387@0:
	push      esi
	add       esp,-12
?live16387@16: ; ESI = result, EAX = args
@73:
	xor       edx,edx
?live16387@32: ; EAX = args
	mov       esi,1
?live16387@48: ; ESI = result, EAX = args
	mov       dword ptr [esp],edx
	mov       cl,1
	mov       dword ptr [esp+4],1072693248
?live16387@96: ; ECX = intf, ESI = result, EAX = args
	mov       eax,dword ptr [eax]
?live16387@112: ; EAX = a, ECX = intf, ESI = result
@74:
	test      eax,eax
	je        @76
@75:
	mov       edx,dword ptr [eax+2]
	test      edx,edx
	je        short @80
	cmp       byte ptr [edx],6
	je        short @78
	cmp       byte ptr [edx],7
	je        short @78
@80:
	xor       edx,edx
	jmp       short @79
@78:
	mov       edx,1
@79:
	test      dl,dl
	jne       short @77
	xor       ecx,ecx
	mov       edx,dword ptr [eax+2]
	mov       ax,5
	call      @@LispError$qqr6LERRORpw4CELLpc
	jmp       @81
@77:
	test      cl,cl
	je        short @82
	mov       edx,dword ptr [eax+2]
	test      edx,edx
	je        short @86
	cmp       byte ptr [edx],7
	je        short @84
@86:
	xor       edx,edx
	jmp       short @85
@84:
	mov       edx,1
@85:
	test      dl,dl
	je        short @83
?live16387@176: ; EAX = a, ESI = result
	xor       ecx,ecx
?live16387@192: ; EAX = a, ECX = intf, ESI = result
	mov       dword ptr [esp+8],esi
	fild      dword ptr [esp+8]
	fstp      qword ptr [esp]
	jmp       short @74
@83:
	mov       edx,dword ptr [eax+2]
	imul      esi,dword ptr [edx+2]
?live16387@240: ; 
 	into	
?live16387@256: ; EAX = a, ECX = intf, ESI = result
	jmp       short @87
@82:
	mov       edx,dword ptr [eax+2]
	test      edx,edx
	je        short @91
	cmp       byte ptr [edx],7
	je        short @89
@91:
	xor       edx,edx
	jmp       short @90
@89:
	mov       edx,1
@90:
	test      dl,dl
	je        short @88
	mov       edx,dword ptr [eax+2]
	fld       qword ptr [edx+2]
	fmul      qword ptr [esp]
	fstp      qword ptr [esp]
	jmp       short @92
@88:
	mov       edx,dword ptr [eax+2]
	fild      dword ptr [edx+2]
	fmul      qword ptr [esp]
	fstp      qword ptr [esp]
@92:
@87:
	mov       eax,dword ptr [eax+6]
	test      eax,eax
	jne       @75
?live16387@352: ; ECX = intf, ESI = result
@76:
	test      cl,cl
	je        short @93
?live16387@368: ; ESI = result
	mov       eax,esi
	call      @@NewInteger$qqri
	jmp       short @81
?live16387@384: ; 
@93:
	push      dword ptr [esp+4]
	push      dword ptr [esp+4]
	call      @@NewReal$qqrd
@94:
@95:
@81:
	add       esp,12
	pop       esi
	ret 
@@Multiply$qqrrpw4CELL	endp
@Multiply$qqrrpw4CELL	ends
_TEXT	ends
_TEXT	segment dword public use32 'CODE'
@Inc$qqrucppw4CELL	segment virtual
	align	2
@@Inc$qqrucppw4CELL	proc	near
?live16388@0:
	push      ebx
	push      esi
	mov       ebx,edx
?live16388@16: ; EBX = args, EAX = nargs
@96:
	cmp       al,1
	jb        short @98
	cmp       al,2
	jbe       short @97
@98:
	xor       ecx,ecx
	mov       edx,dword ptr [_NONE]
	mov       ax,3
	call      @@LispError$qqr6LERRORpw4CELLpc
@114:
	pop       esi
	pop       ebx
	ret 
@97:
	mov       esi,1
?live16388@48: ; EBX = args, ESI = i, EAX = nargs
	cmp       al,2
	jne       short @100
?live16388@64: ; EBX = args
@101:
	mov       eax,dword ptr [ebx+4]
	call      @@iEval$qqrpw4CELL
?live16388@80: ; EBX = args, EAX = delta
	test      eax,eax
	je        short @103
	cmp       byte ptr [eax],6
	je        short @102
@103:
	xor       ecx,ecx
	mov       edx,eax
	mov       ax,6
	call      @@LispError$qqr6LERRORpw4CELLpc
@115:
	pop       esi
	pop       ebx
	ret 
@102:
	mov       esi,dword ptr [eax+2]
?live16388@112: ; EBX = args, ESI = i
@104:
@100:
	mov       eax,dword ptr [ebx]
	test      eax,eax
	je        short @108
	cmp       byte ptr [eax],1
	je        short @106
	cmp       byte ptr [eax],2
	je        short @106
	cmp       byte ptr [eax],3
	je        short @106
@108:
	xor       eax,eax
	jmp       short @107
@106:
	mov       eax,1
@107:
	test      al,al
	jne       short @105
	xor       ecx,ecx
	mov       edx,dword ptr [ebx]
	mov       ax,8
	call      @@LispError$qqr6LERRORpw4CELLpc
@116:
	pop       esi
	pop       ebx
	ret 
@105:
	mov       edx,dword ptr [ebx]
	mov       eax,dword ptr [edx+2]
	test      eax,eax
	je        short @112
	cmp       byte ptr [eax],6
	je        short @110
@112:
	xor       ecx,ecx
	jmp       short @111
@110:
	mov       ecx,1
@111:
	test      cl,cl
	jne       short @109
	mov       ecx,offset s@
	mov       edx,dword ptr [ebx]
	mov       ax,6
	call      @@LispError$qqr6LERRORpw4CELLpc
@109:
	mov       eax,dword ptr [ebx]
	mov       edx,dword ptr [eax+2]
	add       esi,dword ptr [edx+2]
?live16388@176: ; 
 	into	
?live16388@192: ; EBX = args, ESI = i
	mov       eax,esi
	call      @@NewInteger$qqri
?live16388@208: ; EBX = args, EAX = res
	mov       edx,eax
	mov       eax,dword ptr [ebx]
	call      @@iSet$qqrpw4CELLt1
?live16388@224: ; 
@113:
@99:
	pop       esi
	pop       ebx
	ret 
@@Inc$qqrucppw4CELL	endp
@Inc$qqrucppw4CELL	ends
_TEXT	ends
_TEXT	segment dword public use32 'CODE'
@Dec$qqrucppw4CELL	segment virtual
	align	2
@@Dec$qqrucppw4CELL	proc	near
?live16389@0:
	push      ebx
	push      esi
	mov       ebx,edx
?live16389@16: ; EBX = args, EAX = nargs
@117:
	cmp       al,1
	jb        short @119
	cmp       al,2
	jbe       short @118
@119:
	xor       ecx,ecx
	mov       edx,dword ptr [_NONE]
	mov       ax,3
	call      @@LispError$qqr6LERRORpw4CELLpc
@135:
	pop       esi
	pop       ebx
	ret 
@118:
	mov       esi,1
?live16389@48: ; EBX = args, ESI = i, EAX = nargs
	cmp       al,2
	jne       short @121
?live16389@64: ; EBX = args
@122:
	mov       eax,dword ptr [ebx+4]
	call      @@iEval$qqrpw4CELL
?live16389@80: ; EBX = args, EAX = delta
	test      eax,eax
	je        short @124
	cmp       byte ptr [eax],6
	je        short @123
@124:
	xor       ecx,ecx
	mov       edx,eax
	mov       ax,6
	call      @@LispError$qqr6LERRORpw4CELLpc
@136:
	pop       esi
	pop       ebx
	ret 
@123:
	mov       esi,dword ptr [eax+2]
?live16389@112: ; EBX = args, ESI = i
@125:
@121:
	mov       eax,dword ptr [ebx]
	test      eax,eax
	je        short @129
	cmp       byte ptr [eax],1
	je        short @127
	cmp       byte ptr [eax],2
	je        short @127
	cmp       byte ptr [eax],3
	je        short @127
@129:
	xor       eax,eax
	jmp       short @128
@127:
	mov       eax,1
@128:
	test      al,al
	jne       short @126
	xor       ecx,ecx
	mov       edx,dword ptr [ebx]
	mov       ax,8
	call      @@LispError$qqr6LERRORpw4CELLpc
@137:
	pop       esi
	pop       ebx
	ret 
@126:
	mov       edx,dword ptr [ebx]
	mov       eax,dword ptr [edx+2]
	test      eax,eax
	je        short @133
	cmp       byte ptr [eax],6
	je        short @131
@133:
	xor       ecx,ecx
	jmp       short @132
@131:
	mov       ecx,1
@132:
	test      cl,cl
	jne       short @130
	mov       ecx,offset s@
	mov       edx,dword ptr [ebx]
	mov       ax,6
	call      @@LispError$qqr6LERRORpw4CELLpc
@130:
	mov       eax,dword ptr [ebx]
	mov       edx,dword ptr [eax+2]
	mov       ecx,dword ptr [edx+2]
	sub       ecx,esi
	mov       esi,ecx
?live16389@176: ; 
 	into	
?live16389@192: ; EBX = args, ESI = i
	mov       eax,esi
	call      @@NewInteger$qqri
?live16389@208: ; EBX = args, EAX = res
	mov       edx,eax
	mov       eax,dword ptr [ebx]
	call      @@iSet$qqrpw4CELLt1
?live16389@224: ; 
@134:
@120:
	pop       esi
	pop       ebx
	ret 
@@Dec$qqrucppw4CELL	endp
@Dec$qqrucppw4CELL	ends
_TEXT	ends
_DATA	segment dword public use32 'DATA'
s@	label	byte
	;	s@+0:
	db	"Symbol value is not an integer",0
	align	4
_DATA	ends
_TEXT	segment dword public use32 'CODE'
_TEXT	ends
 extrn   _NONE:dword
@@NewReal$qqrd equ @NewReal$qqrd
 extrn   @NewReal$qqrd:near
@@NewInteger$qqri equ @NewInteger$qqri
 extrn   @NewInteger$qqri:near
@@LispError$qqr6LERRORpw4CELLpc equ @LispError$qqr6LERRORpw4CELLpc
 extrn   @LispError$qqr6LERRORpw4CELLpc:near
@@iEval$qqrpw4CELL equ @iEval$qqrpw4CELL
 extrn   @iEval$qqrpw4CELL:near
@@iSet$qqrpw4CELLt1 equ @iSet$qqrpw4CELLt1
 extrn   @iSet$qqrpw4CELLt1:near
	extrn	__turboFloat:word
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\math.h" 10503 12320
	?debug	D "D:\My Documents\Prog\CPP\AJBLisp\V3.7\src\repl.h" 13965 45879
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\time.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\imm.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\mcx.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\winsvc.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\winnetwk.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\winreg.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\winver.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\wincon.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\winnls.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\tvout.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\winuser.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\pshpack1.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\wingdi.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\winerror.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\winbase.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\pshpack8.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\pshpack2.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\poppack.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\pshpack4.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\guiddef.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\basetsd.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\winnt.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\windef.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\excpt.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\windows.h" 10303 12288
	?debug	D "D:\My Documents\Prog\CPP\AJBLisp\V3.7\src\func.h" 14186 12931
	?debug	D "D:\My Documents\Prog\CPP\AJBLisp\V3.7\src\terminal.h" 13909 47815
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\stdarg.h" 10303 12288
	?debug	D "D:\My Documents\Prog\CPP\AJBLisp\V3.7\src\io.h" 13909 47811
	?debug	D "D:\My Documents\Prog\CPP\AJBLisp\V3.7\src\eval.h" 13909 47793
	?debug	D "D:\My Documents\Prog\CPP\AJBLisp\V3.7\src\store.h" 14186 14649
	?debug	D "D:\My Documents\Prog\CPP\AJBLisp\V3.7\src\lisperr.h" 14186 39000
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\mbctype.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\ctype.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\mem.h" 10503 12320
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\_loc.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\locale.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\_str.h" 10503 12320
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\string.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\search.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\stdlib.h" 10503 12320
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\_nfile.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\_null.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\_defs.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\_stddef.h" 10303 12288
	?debug	D "P:\PROGRAM FILES\BORLAND\CBUILDER5\INCLUDE\stdio.h" 10303 12288
	?debug	D "D:\My Documents\Prog\CPP\AJBLisp\V3.7\src\ajblisp.h" 12564 22714
	?debug	D "D:\My Documents\Prog\CPP\AJBLisp\V3.7\src\FuncAsm.cpp" 13909 47832
	end
