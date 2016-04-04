data segment
;Argument storage format:
;storage		:byte[STO_SIZE]			- NULL-terminated arguments
;argc			:byte					- number of parsed arguments
;argptrs		:word[ARGNUM]			- offset of every argument relative to segment
;arglens		:byte[ARGNUM]			- table of arguments' lengths, excluding terminator
;
STO_SIZE = 255d
ARGNUM = STO_SIZE / 2d

storage				db STO_SIZE dup(?)
argc				db 0d
argptrs				dw ARGNUM dup(?)
arglens				db ARGNUM dup(?)


str_overflow		db 'error: arguments too long',13d,10d,13d,10d,'$'

str_usage			db 'usage: kfprint [MOD_FLAG] KEY',13d,10d
					db 'MOD_FLAG = 0|1, default: 0',13d,10d
					db 'KEY = [0-9a-f]{32}',13d,10d,'$'

str_arg_count		db 'error: invalid number of arguments',13d,10d,13d,10d,'$'
str_flag_length		db 'error: invalid flag length',13d,10d,13d,10d,'$'
str_flag_value		db 'error: invalid flag value',13d,10d,13d,10d,'$'
str_key_short		db 'error: key is too short',13d,10d,13d,10d,'$'
str_key_long		db 'error: key is too long',13d,10d,13d,10d,'$'
str_key_value		db 'error: invalid key value',13d,10d,13d,10d,'$'


DEFAULT_FLAG		= 0d
KEY_LENGTH			= 32d

flag				db DEFAULT_FLAG
key					db KEY_LENGTH dup(?)

bit_key				db (KEY_LENGTH / 2d) dup(?)


KFPRINT_HEIGHT 		= 9d
KFPRINT_WIDTH 		= 17d

kfprint				db KFPRINT_HEIGHT dup( KFPRINT_WIDTH dup(0d) )


DEFAULT_START_OFFSET	= KFPRINT_HEIGHT * KFPRINT_WIDTH / 2d

start_offset		db DEFAULT_START_OFFSET		;positions indexed from 0
end_offset			db 0d


BORDER_CHAR_CORNER 			= '+'
BORDER_CHAR_HORIZONTAL 		= '-'
BORDER_CHAR_VERTICAL		= '|'

border_horizontal	db BORDER_CHAR_CORNER, KFPRINT_WIDTH dup(BORDER_CHAR_HORIZONTAL), BORDER_CHAR_CORNER, 13d, 10d, '$'


FREQ_MAX			= 14d
freqmap				db ' .o+=*BOX@%&#/^'
START_CHAR			= 'S'
END_CHAR			= 'E'

data ends


;Storage API;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;LD_STO_SEG	(SEG_REG)
;LD_STO (SEG_REG, REG)
;
;IS_STO_FULL (REG)
;
;ARGCX
;
;ARGP (INDEX, REG)
;ARG (INDEX, REG)
;ARGV (INDEX, REG)
;
;ARGLENP (INDEX, REG)
;ARGLEN (INDEX, REG)
;
;COPY_ARG (SEG_SRC, INDEX, SEG_DEST, OFFSET_DEST)
;

LD_STO_SEG macro SEG_REG				;moves storage segment to SEG_REG
	push ax

	mov ax,seg storage
	mov SEG_REG,ax

	pop ax
endm

LD_STO macro SEG_REG, REG				;moves storage segment to SEG_REG and offset to REG
	LD_STO_SEG SEG_REG
	mov REG,offset storage
endm

IS_STO_FULL macro REG					;checks whether pointer REG is after the last byte of storage data; jae = true / jb = false
	cmp REG,offset argc
endm

ARGCX macro								;moves number of arguments in storage to cx
	push ds

	LD_STO_SEG ds

	mov cl,ds:[argc]
	xor ch,ch

	pop ds
endm

ARGP macro INDEX, REG					;returns in REG pointer to near pointer to argument with index INDEX, indices begin from 0
	xor REG,REG

	mov REG,INDEX
	sal REG,1d

	add REG,offset argc + 1d
endm

ARG macro INDEX, REG					;returns in REG near pointer to argument with index INDEX, indices begin from 0
	push bx								;REG cannot be bx
	push ds

	LD_STO_SEG ds

	ARGP INDEX, REG

	mov bx,REG
	mov REG,ds:[bx]

	pop ds
	pop bx
endm

ARGV macro INDEX, REG					;returns in REG first word of argument with index INDEX, indices begin from 0
	push bx								;REG cannot be bx
	push ds

	LD_STO_SEG ds

	ARGP INDEX, REG

	mov bx,REG
	mov bx,ds:[bx]						;get pointer to argument

	mov bx,ds:[bx]						;get first word of argument
	mov REG,bx

	pop ds
	pop bx
endm

ARGLENP macro INDEX, REG				;returns in REG pointer to size of argument INDEX, indices start from 0
	xor REG,REG
	mov REG,INDEX
	add REG,offset arglens
endm

ARGLEN macro INDEX, REG					;returns in REG size of argument INDEX, indices start from 0
	push bx								;REG cannot be bx
	push ds

	LD_STO_SEG ds

	ARGLENP INDEX, REG

	mov bx,REG
	mov REG,ds:[bx]
	and REG,00ffh

	pop ds
	pop bx
endm

;COPY_ARG
;Copies argument with index INDEX from storage to SEG_DEST:OFFSET_DEST
;
COPY_ARG macro INDEX, SEG_DEST, OFFSET_DEST
	push ax
	push si
	push di
	push ds
	push es

	LD_STO_SEG ds
	ARG INDEX, si

	mov ax,SEG_DEST
	mov es,ax
	mov di,OFFSET_DEST

	ARGLEN INDEX, cx

	rep movsb
	
	pop es
	pop ds
	pop di
	pop si
	pop ax
endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


code segment

;LSTR
;Load address of string NAME to ds:dx
;
;params:	NAME		- name of the string to load
;
LSTR macro NAME
	push ax

	mov ax,seg NAME
	mov ds,ax
	mov dx,offset NAME

	pop ax
endm


;ENDL
;Prints cr lf.
ENDL macro
	push ax
	push dx

	mov ah,02h
	mov dl,0dh
	int 21h
	mov dl,0ah
	int 21h

	pop dx
	pop ax
endm


;error_exit
;If bx isn't 0d, prints $-terminated error string at ds:[dx].
;Exits program with return code set in al.
;
;params:	al			- return code
;			bx			- print error string flag
;			ds:[dx]		- error string
;
ERROR_EXIT macro RET
	mov al,RET
	mov bx,0d
	call $error_exit
endm

ERROR_EXIT_STR macro RET, STR
	mov al,RET
	mov bx,1d
	LSTR STR

	call $error_exit
endm

$error_exit proc
	push ax						;to preserve al return code

	test bx,bx
	jz print_str_usage

	mov ah,09h					;print string at ds:[dx]
	int 21h

print_str_usage:
	LSTR str_usage
	mov ah,09h
	int 21h

	pop ax
	mov ah,4ch
	int 21h
$error_exit endp


;eat_whitespace
;Returns position of the first non-whitespace char in the string
;at ds:[si] and sets ah to 1. If enter (13d) found, sets ah to 0.
;
;params:	ds:[si]		- string to search in
;
;returns:	ds:[si]		- first non-whitespace char found in the string
;			ah = 0d		- enter found
;			ah = 1d		- non-whitespace found
;
eat_whitespace proc
	push ax

get_char:
	lodsb

	cmp al,0dh					;0dh = cr = enter
	je enter_handler

	cmp al,21h					;21h = first non-whitespace ascii char
	jb get_char

	cmp al,7fh					;check delete = 7fh
	je get_char

	dec si						;non-whitespace found
	pop ax
	mov ah,1d
	ret

enter_handler:
	pop ax
	mov ah,0d
	ret
eat_whitespace endp


;copy_arg_to_storage
;Copies string at ds:[si] to storage, beginning at es:[di].
;Updates argptr table and argc. Modifies si, di.
;
;params:	ds:[si]		- source
;			es:[di]		- destination inside a storage
;
;returns:	ds:[si]		- character in the string after the first found whitespace
;			ah = 0d		- enter found
;			ah = 1d		- whitespace found
;
copy_arg_to_storage proc
	push bx
	push dx
	push ax

	IS_STO_FULL di				;check free space
	jae error_overflow

	ARGP word ptr es:[argc], bx	;get pointer to pointer to argument with index argc
	mov es:[bx],di				;save argument ptr

	movsb						;move first char from source to storage
	mov dx,1d					;dx stores length of current argument

get_char:
	lodsb						;load char from source

	cmp al,0dh					;0dh = cr = enter
	je enter_handler

	cmp al,21h					;21h = first non-whitespace ascii char
	jb whitespace_handler

	cmp al,7fh					;check delete = 7fh
	je whitespace_handler

	IS_STO_FULL di				;check free space
	jae error_overflow

	stosb						;move char from al to storage
	inc dx						;increment length

	jmp get_char

enter_handler:
	pop ax						;pop ax now to preserve return value in ah later
	mov ah,0d
	jmp end_arg

whitespace_handler:
	pop ax						;pop ax now to preserve return value in ah later
	mov ah,1d

end_arg:
	IS_STO_FULL di				;check free space
	jae error_overflow

	mov byte ptr es:[di],0d		;terminate argument
	inc di

	ARGLENP word ptr es:[argc], bx	;get pointer to length of argument with index argc
	mov es:[bx],dx				;save argument length

	inc es:[argc]				;increment number of arguments

	pop dx
	pop bx
	ret

error_overflow:
	ERROR_EXIT_STR -1d, str_overflow	;print error and exit if overflow
copy_arg_to_storage endp


;parse_args
;Parses DOS cmdline arguments and saves them in storage.
;
parse_args proc
	push si
	push ds

	mov ah,51h					;get argument segment address from DOS to bx
	int 21h

	mov ds,bx					;DOS segment in bx
	mov si,80h					;80h -> DOS cmdline length

	LD_STO es, di				;init es:di with seg storage:offset storage

	cmp byte ptr ds:[si],0d		;check if there are any arguments
	je warn_no_arguments

	add si,2d					;move si to point at first possible non-whitespace

parse_argument:
	call eat_whitespace

	test ah,ah					;check end of input
	jz return

	call copy_arg_to_storage

	test ah,ah					;check end of input
	jnz parse_argument

return:
	pop ds
	pop si
	ret

warn_no_arguments:				;just print usage info and exit
	ERROR_EXIT 1d
parse_args endp


verify_flag proc
	push ax

	ARGLEN dx, ax				;get length of argument with index dx to ax

	cmp ax,1d					;verify flag length
	jne error_flag_length

	ARGV dx, ax					;get first word of argument with index dx to ax

	cmp	al,'0'					;check if flag is "0"
	je flag_ok

	cmp al,'1'					;check if flag is "1"
	je flag_ok

	ERROR_EXIT_STR -4d, str_flag_value	;else: invalid flag value

flag_ok:
	COPY_ARG dx, seg flag, offset flag	;copy flag from parser storage to dedicated flag variable
		
	pop ax
	ret

error_flag_length:
	ERROR_EXIT_STR -3d, str_flag_length
verify_flag endp


verify_key proc
	push ax
	push cx
	push si

	ARGLEN dx, cx				;get length of argument with index dx to cx

	cmp cx,KEY_LENGTH
	jb error_key_short
	ja error_key_long

	ARG dx, si					;point si at argument with index dx

get_char:
	lodsb

	cmp al,'0'					;check below 0
	jb error_key_value

	cmp al,'9'					;check above 9
	ja not_decimal_digit

	loop get_char

	jmp key_ok

not_decimal_digit:
	cmp al,'f'					;check above f
	ja error_key_value

	cmp al,'a'					;check below a
	jb error_key_value

	loop get_char

key_ok:
	COPY_ARG dx, seg key, offset key	;copy key from parser storage to dedicated key variable

	pop si
	pop cx
	pop ax
	ret

error_key_short:
	ERROR_EXIT_STR -5d, str_key_short

error_key_long:
	ERROR_EXIT_STR -6d, str_key_long

error_key_value:
	ERROR_EXIT_STR -7d, str_key_value
verify_key endp


;verify_args
;
;returns:	exits with negative value if cmdline arguments are not valid
;
verify_args proc
	push ax
	push dx
	push ds

	LD_STO_SEG ds				;load ds with storage segment
	
	mov al,ds:[argc]			;get number of arguments to al
	xor dx,dx					;dx is index of currently verified argument

	cmp al,1d					;one arg?
	je check_key				;leave default flag value and verify key only

	cmp al,2d					;two args?
	je check_flag				;verify flag first

	ERROR_EXIT_STR -2d, str_arg_count	;else: invalid number of arguments

check_flag:
	call verify_flag
	inc dx
check_key:
	call verify_key

	pop ds
	pop dx
	pop ax
	ret
verify_args endp


hex_char_to_bits proc
	cmp al,'9'
	ja not_decimal_digit

	sub al,'0'
	ret

not_decimal_digit:
	cmp al,'F'
	ja not_uppercase

	sub al,'A'-10d
	ret

not_uppercase:
	sub al,'a'-10d
	ret
hex_char_to_bits endp


SALM macro REG, REP
	push cx

	mov cx,REP
salm_loop:
	sal REG,1d
	loop salm_loop

	pop cx
endm


conv_key_to_bits proc
	push ax
	push cx
	push dx
	push si
	push di
	push ds

	mov ax,seg key
	mov ds,ax

	mov si,offset key
	mov di,offset bit_key

	mov cx,KEY_LENGTH / 2d
next_byte:
	mov al,ds:[si]
	inc si
	call hex_char_to_bits

	SALM al,4d

	mov dl,al

	mov al,ds:[si]
	inc si
	call hex_char_to_bits

	add dl,al

	mov ds:[di],dl
	inc di

	loop next_byte

	pop ds
	pop di
	pop si
	pop dx
	pop cx
	pop ax
	ret
conv_key_to_bits endp


OFFSET_TO_COL_ROW macro
	push bx

	mov bl,KFPRINT_WIDTH
	div bl

	pop bx
endm


COL_ROW_TO_OFFSET macro
	push bx

	mov bh,ah					;preserve column index

	mov bl,KFPRINT_WIDTH
	mul bl

	add al,bh
	xor ah,ah

	pop bx
endm


perform_move proc
	OFFSET_TO_COL_ROW			;ah = column index, al = row index

	test dh,dl					;check bit
	jnz move_right

	cmp ah,0d					;check left bound
	jle vertical_bit

	dec ah

	jmp vertical_bit
move_right:
	cmp ah,KFPRINT_WIDTH - 1d	;check right bound
	jge vertical_bit

	inc ah
vertical_bit:
	sal dl,1d					;adjust bit mask

	test dh,dl					;check bit
	jnz move_down

	cmp al,0d					;check top bound
	jle end_move

	dec al

	jmp end_move
move_down:
	cmp al,KFPRINT_HEIGHT - 1d	;check bottom bound
	jge end_move

	inc al
end_move:
	sal dl,1d					;adjust bit mask
	COL_ROW_TO_OFFSET			;ax = ah + KFPRINT_WIDTH * al
	ret
perform_move endp


perform_mod_move proc
	OFFSET_TO_COL_ROW			;ah = column index, al = row index

	test dh,dl					;check bit
	jnz move_right

	dec ah

	cmp ah,0d					;check left bound
	jge vertical_bit

	add ah,KFPRINT_WIDTH

	jmp vertical_bit
move_right:
	inc ah

	cmp ah,KFPRINT_WIDTH - 1d	;check right bound
	jle vertical_bit

	sub ah,KFPRINT_WIDTH
vertical_bit:
	sal dl,1d					;adjust bit mask

	test dh,dl					;check bit
	jnz move_down

	dec al

	cmp al,0d					;check top bound
	jge end_move

	add al,KFPRINT_HEIGHT

	jmp end_move
move_down:
	inc al

	cmp al,KFPRINT_HEIGHT - 1d	;check bottom bound
	jle end_move

	sub al,KFPRINT_HEIGHT
end_move:
	sal dl,1d					;adjust bit mask
	COL_ROW_TO_OFFSET			;ax = ah + KFPRINT_WIDTH * al
	ret
perform_mod_move endp


move_bishop proc
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push ds

	mov ax,seg bit_key
	mov ds,ax
	mov bx,offset bit_key

	mov al,ds:[start_offset]
	xor ah,ah

	mov cx,KEY_LENGTH / 2d

	cmp ds:[flag],'1'			;check modification
	je modification

	mov di,offset perform_move
	jmp next_byte
modification:
	mov di,offset perform_mod_move

next_byte:
	mov dl,01h					;bit mask
	mov dh,ds:[bx]				;load byte with moves
	inc bx

	push cx
	mov cx,4d					;4 moves in each byte
next_move:
	call di

	mov si,ax					;ax = offset after move
	add si,offset kfprint
	inc byte ptr ds:[si]		;increment field's frequency

	loop next_move
	pop cx
	loop next_byte

	mov ds:[end_offset],al		;set end offset

	pop ds
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
move_bishop endp


map_freq_to_chars proc
	push ax
	push bx
	push cx
	push ds

	mov ax,seg kfprint
	mov ds,ax

	xor bh,bh
	mov si,offset kfprint
	mov cx,KFPRINT_HEIGHT * KFPRINT_WIDTH
map_char:
	mov bl,ds:[si]				;get frequency

	cmp bl,FREQ_MAX				;check too big frequency
	jb no_freq_swap
	mov bl,FREQ_MAX

no_freq_swap:
	mov al,ds:[freqmap + bx]	;get frequency char
	mov ds:[si],al				;replace frequency with its char
	inc si
	loop map_char

	mov bl,ds:[start_offset]
	mov byte ptr ds:[offset kfprint + bx],START_CHAR
	mov bl,ds:[end_offset]
	mov byte ptr ds:[offset kfprint + bx],END_CHAR

	pop ds
	pop cx
	pop bx
	pop ax
	ret
map_freq_to_chars endp


print_kfprint proc
	push ax
	push cx
	push dx
	push ds

	mov ax,seg kfprint
	mov ds,ax

	mov ah,09h
	mov dx,offset border_horizontal
	int 21h

	mov ah,02h
	mov bx,offset kfprint
	mov cx,KFPRINT_HEIGHT
print_row:
	mov dl,BORDER_CHAR_VERTICAL
	int 21h

	push cx
	mov cx,KFPRINT_WIDTH

print_char:
	mov dl,ds:[bx]
	inc bx
	int 21h	
	loop print_char
	
	pop cx

	mov dl,BORDER_CHAR_VERTICAL
	int 21h
	ENDL

	loop print_row

	mov ah,09h
	mov dx,offset border_horizontal
	int 21h

	pop ds
	pop dx
	pop cx
	pop ax
	ret
print_kfprint endp


main:
	mov ax,stack				;init stack segment
	mov ss,ax
	mov sp,offset top

	call parse_args
	
	call verify_args
	
	call conv_key_to_bits

	call move_bishop

	call map_freq_to_chars

	call print_kfprint

	mov ax,4c00h				;return 0
	int 21h
code ends


stack segment stack

	db 1024d dup(?)
top	db ?

stack ends

end main
