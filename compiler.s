%define T_UNDEFINED 0
%define T_VOID 1
%define T_NIL 2
%define T_RATIONAL 3
%define T_FLOAT 4
%define T_BOOL 5
%define T_CHAR 6
%define T_STRING 7
%define T_SYMBOL 8
%define T_CLOSURE 9
%define T_PAIR 10
%define T_VECTOR 11

%define TYPE_SIZE 1
%define WORD_SIZE 8
	
%define KB(n) n*1024
%define MB(n) 1024*KB(n)
%define GB(n) 1024*MB(n)

%macro SKIP_TYPE_TAG 2
	mov %1, qword [%2+TYPE_SIZE]	
%endmacro	

%define NUMERATOR SKIP_TYPE_TAG

%macro DENOMINATOR 2
	mov %1, qword [%2+TYPE_SIZE+WORD_SIZE]
%endmacro

%macro CHAR_VAL 2
	movzx %1, byte [%2+TYPE_SIZE]
%endmacro

%define FLOAT_VAL SKIP_TYPE_TAG

%define STRING_LENGTH SKIP_TYPE_TAG
%define VECTOR_LENGTH SKIP_TYPE_TAG

%define SYMBOL_VAL SKIP_TYPE_TAG

%macro STRING_ELEMENTS 2
	lea %1, [%2+TYPE_SIZE+WORD_SIZE]
%endmacro
%define VECTOR_ELEMENTS STRING_ELEMENTS

%define CAR SKIP_TYPE_TAG

%macro CDR 2
	mov %1, qword [%2+TYPE_SIZE+WORD_SIZE]
%endmacro

%define CLOSURE_ENV CAR

%define CLOSURE_CODE CDR

%define PVAR(n) qword [rbp+(4+n)*WORD_SIZE]

; returns %2 allocated bytes in register %1
; Supports using with %1 = %2
%macro MALLOC 2
	add qword [malloc_pointer], %2
	push %2
	mov %1, qword [malloc_pointer]
	sub %1, [rsp]
	add rsp, 8
%endmacro
	
; Creates a short SOB with the
; value %2
; Returns the result in register %1
%macro MAKE_CHAR_VALUE 2
	MALLOC %1, 1+TYPE_SIZE
	mov byte [%1], T_CHAR
	mov byte [%1+TYPE_SIZE], %2
%endmacro

; Creates a long SOB with the
; value %2 and type %3.
; Returns the result in register %1
%macro MAKE_LONG_VALUE 3
	MALLOC %1, TYPE_SIZE+WORD_SIZE
	mov byte [%1], %3
	mov qword [%1+TYPE_SIZE], %2
%endmacro

%define MAKE_FLOAT(r,val) MAKE_LONG_VALUE r, val, T_FLOAT
%define MAKE_CHAR(r,val) MAKE_CHAR_VALUE r, val

; Create a string of length %2
; from char %3.
; Stores result in register %1
%macro MAKE_STRING 3
	lea %1, [%2+WORD_SIZE+TYPE_SIZE]
	MALLOC %1, %1
	mov byte [%1], T_STRING
	mov qword [%1+TYPE_SIZE], %2
	push rcx
	add %1,WORD_SIZE+TYPE_SIZE
	mov rcx, %2
	cmp rcx, 0
%%str_loop:
	jz %%str_loop_end
	dec rcx
	mov byte [%1+rcx], %3
	jmp %%str_loop
%%str_loop_end:
	pop rcx
	sub %1, WORD_SIZE+TYPE_SIZE
%endmacro

; Create a vector of length %2
; from array of elements in register %3
; Store result in register %1
%macro MAKE_VECTOR 3
	lea %1, [%2+WORD_SIZE+TYPE_SIZE]
	MALLOC %1, %1
	mov byte [%1], T_VECTOR
	mov qword [%1+TYPE_SIZE], %2

    push rbx
    push rcx
    push %1
    add %1,WORD_SIZE+TYPE_SIZE
    mov rcx, %2
%%vector_loop:
    cmp rcx, 0
    js %%vector_loop_end
    mov rbx, [%3]
    mov [%1], rbx
    add %1, WORD_SIZE
    add %3, WORD_SIZE
    dec rcx
    jmp %%vector_loop
%%vector_loop_end:
    pop %1
    pop rcx
    pop rbx
%endmacro

;;; Creates a SOB with tag %2 
;;; from two pointers %3 and %4
;;; Stores result in register %1
%macro MAKE_TWO_WORDS 4 
        MALLOC %1, TYPE_SIZE+WORD_SIZE*2
        mov byte [%1], %2
        mov qword [%1+TYPE_SIZE], %3
        mov qword [%1+TYPE_SIZE+WORD_SIZE], %4
%endmacro

%macro MAKE_WORDS_LIT 3
	db %1
        dq %2
        dq %3
%endmacro

%macro MAKE_WORD_LIT 2
	db %1
        dq %2
%endmacro

%macro MAKE_WORD_LIT_CHAR 2
	db %1
        db %2
%endmacro

%macro MAKE_LITERAL_STRING 1
	db T_STRING
	dq (%%end_str- %%str)
%%str:
	db %1
%%end_str:
%endmacro

%macro MAKE_LITERAL_VECTOR 0-*
	db T_VECTOR
	dq %0
%rep %0
	dq %1
%rotate 1
%endrep
%endmacro

%define MAKE_RATIONAL(r, num, den) \
	MAKE_TWO_WORDS r, T_RATIONAL, num, den

%define MAKE_LITERAL_RATIONAL(num, den) \
	MAKE_WORDS_LIT T_RATIONAL, num, den

%define MAKE_LITERAL_FLOAT(num) \
	MAKE_WORD_LIT T_FLOAT, num

%define MAKE_LITERAL_CHAR(ch) \
	MAKE_WORD_LIT_CHAR T_CHAR, ch

%define MAKE_LITERAL_SYMBOL(sym) \
	MAKE_WORD_LIT T_SYMBOL, sym
	
%define MAKE_PAIR(r, car, cdr) \
        MAKE_TWO_WORDS r, T_PAIR, car, cdr

%define MAKE_LITERAL_PAIR(car, cdr) \
        MAKE_WORDS_LIT T_PAIR, car, cdr

%define MAKE_CLOSURE(r, env, body) \
        MAKE_TWO_WORDS r, T_CLOSURE, env, body

%define PARAM_COUNT qword [rbp+3*WORD_SIZE]

%macro ADJUST_STUCK 1 								;; vars + 1 for opt 
	mov rax, PARAM_COUNT 							;; number of all the paramaters (vars+all in opt) 
	cmp rax, %1
	jl %%End_Final									;; case that will not happend / its ok because of the magic
	mov rax, PARAM_COUNT 							;; number of all the paramaters (vars+all in opt) 
	cmp rax, %1
	jne %%There_is_Opt
	mov rdi, PARAM_COUNT
	add rdi, 3
	shl rdi, 3
	add rdi, rbp
	mov r9, [rdi]
	MAKE_PAIR (r11, r9, SOB_NIL_ADDRESS)			;; in this case we get all the vars with opt of size 1 so we make this opt to pair with nil
	mov [rdi], r11
	jmp %%End_Final									;; after the pair we can end
	%%There_is_Opt:									;; there is opt in size more than one and we want to make it to a list
	mov r9, rax
	add r9, 3
	shl r9, 3
	add r9, rbp
	mov r10, [r9]		
	MAKE_PAIR (r11, r10, SOB_NIL_ADDRESS)			;; pair of the last object in opt vals with nil 
	mov rdi, rax
	sub rdi, %1
	%%Make_Opt_List:								;; make list of all the opt vars
	cmp rdi, 0
	je %%Make_Opt_List_End
	sub r9, WORD_SIZE
	mov r10, [r9]
	MAKE_PAIR (rsi, r10, r11)
	mov r11, rsi
	dec rdi
	jmp %%Make_Opt_List
	%%Make_Opt_List_End:
	mov [r9], r11  									;; move the list of opt to the first place after the vars 
	mov rdi, rax
	sub rdi, %1										;; the number of cells that we need to move each time
	mov r10, 0										;; the index to the moveing loop 
	%%Adjust_The_Stack:
	mov r11, %1
	add r11, 4										;; the number of the cells that we need to move (4 regular + vars + 1 for the list 
	cmp r10, r11
	je %%Adjust_The_Stack_End
	mov rsi, [r9]
	mov [r9+rdi*WORD_SIZE], rsi
	sub r9, WORD_SIZE
	inc r10
	jmp %%Adjust_The_Stack
	%%Adjust_The_Stack_End:
	shl rdi, 3
	add rbp, rdi									;; update rbp to the correct place (old rbp)
	add rsp, rdi
	shr rdi, 3
	mov r10 , %1
	mov [rbp+3*WORD_SIZE], r10						;; update new param count to the correct place (vars + 1 opt for the list) 
	%%End_Final:
%endmacro

%macro SHIFT_FRAME 1               
	push PARAM_COUNT
	push rsi
	push rax
	push r9
	push r11
	push rdi
	mov r11, PARAM_COUNT
	add r11, 5										;; index for the stack
	mov rsi, [rbp]									;; saving old rbp
	mov rax, 1										;; loop index		
	%%Start_Shift:
	cmp rax, %1										;; 4 + number of paramaters (without the old rbp)
	jg %%End_Shift									;; start the overriding
	mov rdi, rbp
	mov r9, rax
	inc rax									
	shl r9, 3
	sub rdi, r9
	push qword[rdi]									;; pushing object from the stack and after that we will override with him another object
	dec r11
	mov r9, r11								
	shl r9, 3
	mov rdi, rbp
	add rdi, r9
	pop qword[rdi]									;; overriding
	jmp %%Start_Shift
	%%End_Shift:
	mov rbp, rsi
	pop rdi
	pop r11
	pop r9
	pop rax
	pop rsi
	pop r10											;; pop the param count of the old frame and then add 5 and mul 8 for updating the rsp
	add r10, 5
	shl r10, 3
	add rsp, r10
%endmacro

%macro APPLY 0
	push rax
	push r10
	push r11
	push rdi
	push rsi
	mov rax, rsp                     				;; the beginning of the new frame (of proc)
	push SOB_NIL_ADDRESS             				;; Nil (magic) for the proc frame
	mov r9, PARAM_COUNT
	add r9, 3
	shl r9, 3
	add r9, rbp
	mov r9, [r9]		               				;; list of argumetns of apply frame
	mov r10, 0
	%%Pushing_Last_List:               				;; push to proc frame the last list of the arguments (s)
	cmp r9, SOB_NIL_ADDRESS
	je %%Pushing_Last_List_End
	CAR r11, r9
	CDR r9, r9
	inc r10											;; r10 = the number of the objects in the ending list
	push r11
	jmp %%Pushing_Last_List
	%%Pushing_Last_List_End:
	REVERSE_LIST                             		;; reversing the last list of the arguments (s)
	mov r9, PARAM_COUNT
	add r9, 2
	shl r9, 3
	add r9, rbp									;; point to the x_n (PARAM_COUNT+4 regular-2(nil+last list))
	mov rsi, PARAM_COUNT
	sub rsi, 2										;; the number of x1...xn (PARAM_COUNT-1(proc)-1(list))
	%%Push_Args:               						;; push to proc frame the rest arguments (x1,....)
	cmp rsi, 0
	je %%Push_Args_End
	push qword[r9]
	sub r9, WORD_SIZE
	dec rsi
	jmp %%Push_Args
	%%Push_Args_End:
	mov rsi, PARAM_COUNT
	add rsi, r10
	sub rsi, 2
	push rsi                         				;; push to proc frame the number of arguments to the proc
	mov r9, [rbp+4*WORD_SIZE]       				;; clouser of the proc that apply get
	push qword[r9+1]                				;; go to the closure of proc (+1 for the tag) - and get the env of the proc
	push qword[rbp+WORD_SIZE]        				;; ret address
	push qword[rbp]			         				;; old rbp
	push rsi				               			;; new args number to send to SHIFT_APPLY
	mov r9, rsp                     				;; the top of the new frame (of proc)
	mov rsp, rax                     				;; the beginning of the new frame (of proc)
	pop rsi
	pop rdi
	pop r11
	pop r10
	pop rax
	mov rsp, r9                     				;; rsp go back to the top of the new frame (of proc)
	pop rsi                          				;; put in rsi the number of the arguments for proc
	SHIFT_FRAME_APPLY rsi
	pop rbp                          				;; because we have "push rbp" in any start of code of proc that we go there in the next line
	mov r9, [r9+9]		            			;; proc code (closure + 1 tag + 8 env = code)
	jmp r9                          				;; go FINALLY to the code of proc
%endmacro

%macro REVERSE_LIST 0                      			;; reversing the last argument (s - list)
	push rax
	push r9
	push rdi
	push rsi
	mov rdi, r10									;; number of objects in the list
	mov rax, -7										
	shl rax, 3
	add rax, rbp									;; we will go to the first object (5(registers)+1(nil)+1)
	mov r9, rax
	shl r10, 3
	sub r9, r10										;; here is one above the last object in the list								
	shr r10, 3
	%%Loop_Pushing:									;; pushing all the list to the top of the stack
	cmp rax, r9
	je %%Loop_Pushing_End
	push qword[rax]
	sub rax, WORD_SIZE
	jmp %%Loop_Pushing
	%%Loop_Pushing_End:
	shl r10, 3
	add r9, r10										
	shr r10, 3 
	%%Loop_Popping:									;; popping the list reverse to the stack
	cmp r9, rax
	je %%Loop_Popping_End
	pop qword[r9]
	sub r9, WORD_SIZE
	jmp %%Loop_Popping
	%%Loop_Popping_End:
	pop rsi
	pop rdi
	pop r9
	pop rax
%endmacro

%macro SHIFT_FRAME_APPLY 1               			;; copy the new frame (of proc) with overiding the old frame (of apply)
	push r10
	push r11
	push rdi
	push rsi
	mov r9, [rbp+4*WORD_SIZE]	      				;; the closure of the proc of apply
	mov rdi, PARAM_COUNT             				;; the number of args of apply frame
	add rdi, 10				            			;; the number of args of apply frame + 10 (5 registers that we popped and 5 regular cells of any frame) = the size that we want to override
	mov rsi, %1                      				;; the number of args of the new frame (of proc)
	add rsi, 5                       				;; the number of args of the new frame (of proc) + 5 of any frame = the size of the new frame (of proc)
	mov rax, rsi                     				;; the size of the new frame (of proc)
	mov r10, rbp                     				;; top of the apply frame          
	sub r10, 48                      				;; top of the apply frame - 5 registers that we popped = rbp-6*8
	%%Copy_Frame:                      				;; copy the new frame (of proc) with overriden the old frame (of apply)
	cmp rsi, 0
	je %%Copy_Frame_End
	mov r11, [r10]
	mov [r10+rdi*WORD_SIZE], r11						
	sub r10, WORD_SIZE
	dec rsi
	jmp %%Copy_Frame
	%%Copy_Frame_End:
	add r10, WORD_SIZE
	shl rdi, 3
	add r10, rdi                     				;; getting rbp
	mov rax, r10
	pop rsi
	pop rdi
	pop r11
	pop r10
	mov rsp, rax                     				;; rsp = rax = r10 = old rbp ---> rsp is where the top of the new frame (of proc))
%endmacro

;;; Macros and routines for printing Scheme OBjects to STDOUT
%define CHAR_NUL 0
%define CHAR_TAB 9
%define CHAR_NEWLINE 10
%define CHAR_PAGE 12
%define CHAR_RETURN 13
%define CHAR_SPACE 32
%define CHAR_DOUBLEQUOTE 34
%define CHAR_BACKSLASH 92
	
extern printf, malloc
global write_sob, write_sob_if_not_void
	
write_sob_undefined:
	push rbp
	mov rbp, rsp

	mov rax, qword 0
	mov rdi, .undefined
	call printf

	pop rbp
	ret

section .data
.undefined:
	db "#<undefined>", 0

section .text
write_sob_rational:
	push rbp
	mov rbp, rsp

	mov rdx, rsi
	NUMERATOR rsi, rdx
	DENOMINATOR rdx, rdx
	
	cmp rdx, 1
	jne .print_fraction

	mov rdi, .int_format_string
	jmp .print

.print_fraction:
	mov rdi, .frac_format_string

.print:	
	mov rax, 0
	call printf

	pop rbp
	ret

section .data
.int_format_string:
	db "%ld", 0
.frac_format_string:
	db "%ld/%ld", 0

section .text
write_sob_float:
	push rbp
	mov rbp, rsp

	FLOAT_VAL rsi, rsi
	movq xmm0, rsi
	mov rdi, .float_format_string
	mov rax, 1

	;; printf-ing floats (among other things) requires the stack be 16-byte aligned
	;; so align the stack *downwards* (take up some extra space) if needed before
	;; calling printf for floats
	and rsp, -16 
	call printf

	;; move the stack back to the way it was, cause we messed it up in order to
	;; call printf.
	;; Note that the `leave` instruction does exactly this (reset the stack and pop
	;; rbp). The instructions are explicitly layed out here for clarity.
	mov rsp, rbp
	pop rbp
	ret
	
section .data
.float_format_string:
	db "%f", 0		

section .text
write_sob_char:
	push rbp
	mov rbp, rsp

	CHAR_VAL rsi, rsi

	cmp rsi, CHAR_NUL
	je .Lnul

	cmp rsi, CHAR_TAB
	je .Ltab

	cmp rsi, CHAR_NEWLINE
	je .Lnewline

	cmp rsi, CHAR_PAGE
	je .Lpage

	cmp rsi, CHAR_RETURN
	je .Lreturn

	cmp rsi, CHAR_SPACE
	je .Lspace
	jg .Lregular

	mov rdi, .special
	jmp .done	

.Lnul:
	mov rdi, .nul
	jmp .done

.Ltab:
	mov rdi, .tab
	jmp .done

.Lnewline:
	mov rdi, .newline
	jmp .done

.Lpage:
	mov rdi, .page
	jmp .done

.Lreturn:
	mov rdi, .return
	jmp .done

.Lspace:
	mov rdi, .space
	jmp .done

.Lregular:
	mov rdi, .regular
	jmp .done

.done:
	mov rax, 0
	call printf

	pop rbp
	ret

section .data
.space:
	db "#\space", 0
.newline:
	db "#\newline", 0
.return:
	db "#\return", 0
.tab:
	db "#\tab", 0
.page:
	db "#\page", 0
.nul:
	db "#\nul", 0
.special:
	db "#\x%02x", 0
.regular:
	db "#\%c", 0

section .text
write_sob_void:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .void
	call printf

	pop rbp
	ret

section .data
.void:
	db "#<void>", 0
	
section .text
write_sob_bool:
	push rbp
	mov rbp, rsp

	cmp word [rsi], word T_BOOL
	je .sobFalse
	
	mov rdi, .true
	jmp .continue

.sobFalse:
	mov rdi, .false

.continue:
	mov rax, 0
	call printf	

	pop rbp
	ret

section .data			
.false:
	db "#f", 0
.true:
	db "#t", 0

section .text
write_sob_nil:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .nil
	call printf

	pop rbp
	ret

section .data
.nil:
	db "()", 0

section .text
write_sob_string:
	push rbp
	mov rbp, rsp

	push rsi

	mov rax, 0
	mov rdi, .double_quote
	call printf
	
	pop rsi

	STRING_LENGTH rcx, rsi
	STRING_ELEMENTS rax, rsi

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	cmp rbx, CHAR_TAB
	je .ch_tab
	cmp rbx, CHAR_NEWLINE
	je .ch_newline
	cmp rbx, CHAR_PAGE
	je .ch_page
	cmp rbx, CHAR_RETURN
	je .ch_return
	cmp rbx, CHAR_DOUBLEQUOTE
	je .ch_doublequote
	cmp rbx, CHAR_BACKSLASH
	je .ch_backslash
	cmp rbx, CHAR_SPACE
	jl .ch_hex
	
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx
	jmp .printf
	
.ch_tab:
	mov rdi, .fs_tab
	mov rsi, rbx
	jmp .printf
	
.ch_page:
	mov rdi, .fs_page
	mov rsi, rbx
	jmp .printf
	
.ch_return:
	mov rdi, .fs_return
	mov rsi, rbx
	jmp .printf

.ch_newline:
	mov rdi, .fs_newline
	mov rsi, rbx
	jmp .printf

.ch_doublequote:
	mov rdi, .fs_doublequote
	mov rsi, rbx
	jmp .printf

.ch_backslash:
	mov rdi, .fs_backslash
	mov rsi, rbx

.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	mov rax, 0
	mov rdi, .double_quote
	call printf

	pop rbp
	ret
section .data
.double_quote:
	db CHAR_DOUBLEQUOTE, 0
.fs_simple_char:
	db "%c", 0
.fs_hex_char:
	db "\x%02x;", 0	
.fs_tab:
	db "\t", 0
.fs_page:
	db "\f", 0
.fs_return:
	db "\r", 0
.fs_newline:
	db "\n", 0
.fs_doublequote:
	db CHAR_BACKSLASH, CHAR_DOUBLEQUOTE, 0
.fs_backslash:
	db CHAR_BACKSLASH, CHAR_BACKSLASH, 0

section .text
write_sob_pair:
	push rbp
	mov rbp, rsp

	push rsi
	
	mov rax, 0
	mov rdi, .open_paren
	call printf

	mov rsi, [rsp]

	CAR rsi, rsi
	call write_sob

	mov rsi, [rsp]
	CDR rsi, rsi
	call write_sob_pair_on_cdr
	
	add rsp, 1*8
	
	mov rdi, .close_paren
	mov rax, 0
	call printf

	pop rbp
	ret

section .data
.open_paren:
	db "(", 0
.close_paren:
	db ")", 0

section .text
write_sob_pair_on_cdr:
	push rbp
	mov rbp, rsp

	mov bl, byte [rsi]
	cmp bl, T_NIL
	je .done
	
	cmp bl, T_PAIR
	je .cdrIsPair
	
	push rsi
	
	mov rax, 0
	mov rdi, .dot
	call printf
	
	pop rsi

	call write_sob
	jmp .done

.cdrIsPair:
	CDR rbx, rsi
	push rbx
	CAR rsi, rsi
	push rsi
	
	mov rax, 0
	mov rdi, .space
	call printf
	
	pop rsi
	call write_sob

	pop rsi
	call write_sob_pair_on_cdr

.done:
	pop rbp
	ret

section .data
.space:
	db " ", 0
.dot:
	db " . ", 0

section .text
write_sob_symbol:
	push rbp
	mov rbp, rsp

	SYMBOL_VAL rsi, rsi
	
	STRING_LENGTH rcx, rsi
	STRING_ELEMENTS rax, rsi

	mov rdx, rcx

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	cmp rcx, rdx
	jne .ch_simple
	cmp rbx, '+'
	je .ch_hex
	cmp rbx, '-'
	je .ch_hex
	cmp rbx, 'A'
	jl .ch_hex

.ch_simple:
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx

.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	pop rbp
	ret
	
section .data
.fs_simple_char:
	db "%c", 0
.fs_hex_char:
	db "\x%02x;", 0	

section .text
write_sob_closure:
	push rbp
	mov rbp, rsp

	CLOSURE_CODE rdx, rsi
	CLOSURE_ENV rsi, rsi

	mov rdi, .closure
	mov rax, 0
	call printf

	pop rbp
	ret
section .data
.closure:
	db "#<closure [env:%p, code:%p]>", 0

section .text
write_sob_vector:
    push rbp
    mov rbp, rsp

    push rsi

    mov rax, 0
    mov rdi, .vector_open_paren
    call printf

    mov rsi, [rsp]

    SKIP_TYPE_TAG rcx, rsi
    VECTOR_ELEMENTS rax, rsi

.loop:
    cmp rcx, 0
    je .done

    mov rsi, [rax]
    push rax
    push rcx
    call write_sob
    pop rcx
    pop rax

    dec rcx
    jz .done

    push rax
    push rcx
    mov rax, 0
    mov rdi, .vector_space
    call printf
    pop rcx
    pop rax

    add rax, WORD_SIZE
    jmp .loop

.done:
    mov rax, 0
    mov rdi, .vector_close_paren
    call printf

    pop rsi

    pop rbp
    ret

section .data
.vector_open_paren:
    db "#(", 0

.vector_space:
    db " ", 0

.vector_close_paren:
    db ")", 0

section .text
write_sob:
	mov rbx, 0
	mov bl, byte [rsi]	
	jmp qword [.jmp_table + rbx * 8]

section .data
.jmp_table:
	dq write_sob_undefined, write_sob_void, write_sob_nil
	dq write_sob_rational, write_sob_float, write_sob_bool
	dq write_sob_char, write_sob_string, write_sob_symbol
	dq write_sob_closure, write_sob_pair, write_sob_vector

section .text
write_sob_if_not_void:
	mov rsi, rax
	mov bl, byte [rsi]
	cmp bl, T_VOID
	je .continue

	call write_sob
	
	mov rax, 0
	mov rdi, .newline
	call printf
	
.continue:
	ret
section .data
.newline:
	db CHAR_NEWLINE, 0
