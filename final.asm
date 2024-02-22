.model tiny
.286
.code

org 100h

Start:  cld                         ; reading frame params from cmd
        mov si, 82h
        call ReadFrameParams

        mov FrameColor,  ax
        mov FrameWidth,  bx
        mov FrameHeight, cx

        call ReadFrameStyle
        mov FrameStyleOffset, dx

        mov ax, 3509h       ; saving old 09 int adr
        int 21h
        mov Old09Ofs, bx
        mov bx, es
        mov Old09Seg, bx

        mov ax, 3508h       ; saving old 08 int adr   
        int 21h
        mov Old08Ofs, bx
        mov bx, es
        mov Old08Seg, bx

        mov ax, 0           ; moving new 09 and 08 int's adr into int's table
        mov es, ax
        mov ax, cs
        mov bx, 9 * 4
        cli
        mov es:[bx], offset New09Interrupt
        mov es:[bx + 2], ax

        sub bx, 4       ; bx = 8 * 4
        mov es:[bx], offset New08Interrupt
        mov es:[bx + 2], ax
        sti

        mov ax, 0

        mov ax, 3100h
        mov dx, offset EOP
        shr dx, 4
        inc dx
        int 21h

;------------------------------------------------
;09 interruption 
;------------------------------------------------
New09Interrupt   proc
        push ax

        in al,  60h
        cmp al, 44h ; scancode of F10
        jne Old09Interrupt

        pop ax

        ; previous program sp is sp + 6 (skip seg + ofs + flags) 

        push sp ax bx cx dx si di bp ds es ss
        call PrintFrameWithRegisters
        pop ss es ds bp di si dx cx bx ax sp 

        mov cs:FrameIsShown, 1
        push ax 

        in al, 61h      ; blink high bit in 61h port
        or al, 80h
        out 61h, al
        and al, not 80h
        out 61h, al

        mov al, 20h     ; EOI
        out 20h, al

        pop ax
        iret

Old09Interrupt:
        pop ax

        db 0eah         ; jump far
        Old09Ofs dw 0
        Old09Seg dw 0

        endp

FrameIsShown db 0             ; bool variable - frame is shown or not


;------------------------------------------------
;08 interruption 
;------------------------------------------------
New08Interrupt   proc
        cmp cs:FrameIsShown, 0
        je Old08Interrupt

        push sp ax bx cx dx si di bp ds es ss
        call PrintFrameWithRegisters
        pop ss es ds bp di si dx cx bx ax sp

Old08Interrupt:
        db 0eah         ; jump far
        Old08Ofs dw 0
        Old08Seg dw 0

        endp
        

;------------------------------------------------
;Prints frame in the top left corner and registers in it
;Entry: registers on stack, push order:
;       push [flags cs ip] sp ax bx cx dx si di bp ds es ss [ret adr]
;       also registers info [ax, ss] is saved in them
;       cs = [bp + 28],
;       ip = [bp + 26]
;Exit:  None
;Destr: All registers
;------------------------------------------------
PrintFrameWithRegisters proc
        push ax bx cx dx di si es ds
        mov ax, cs:FrameColor
        mov bx, cs:FrameWidth
        mov cx, cs:FrameHeight
        mov dx, cs
        mov ds, dx
        mov si, cs:FrameStyleOffset
        cld
        call PrintFrame

        pop ds es si di dx cx bx ax
        
        call PrintRegisters

        ret
        endp


;------------------------------------------------
;Print registers
;Entry: Registers,
;       push [flags cs ip] sp ax bx cx dx si di bp ds es ss [smth] [ret adr]
;       sp = [bp + 26] + 6,
;       cs = [bp + 30],
;       ip = [bp + 28], (using bp as in stack frame)
;Exit : None
;Destr: all registers
;------------------------------------------------   
PrintRegisters proc
        push bp
        mov bp, sp

        COLUMN_LEFT_SHIFT equ 4d
        mov di, 0b800h
        mov es, di
        mov di, 1 * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT
        
        mov es:[di], byte ptr 'a'
        add di, 2
        mov es:[di], byte ptr 'x'
        add di, 4
        call PrintRegisterValue
        mov di, 2 * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT

        mov ax, [bp + 22d]          ; popping previous bx to ax
        mov es:[di], byte ptr 'b'
        add di, 2
        mov es:[di], byte ptr 'x'
        add di, 4
        call PrintRegisterValue
        mov di, 3 * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT

        mov ax, cx
        mov es:[di], byte ptr 'c'
        add di, 2
        mov es:[di], byte ptr 'x'
        add di, 4
        call PrintRegisterValue
        mov di, 4 * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT

        mov ax, dx
        mov es:[di], byte ptr 'd'
        add di, 2
        mov es:[di], byte ptr 'x'
        add di, 4
        call PrintRegisterValue
        mov di, 5 * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT

        mov ax, si
        mov es:[di], byte ptr 's'
        add di, 2
        mov es:[di], byte ptr 'i'
        add di, 4
        call PrintRegisterValue
        mov di, 6 * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT

        mov ax, [bp + 14d]       ; popping real di to ax
        mov es:[di], byte ptr 'd'
        add di, 2
        mov es:[di], byte ptr 'i'
        add di, 4
        call PrintRegisterValue
        mov di, 7 * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT
        
        mov ax, [bp]        ; real bp
        mov es:[di], byte ptr 'b'
        add di, 2
        mov es:[di], byte ptr 'p'
        add di, 4
        call PrintRegisterValue
        mov di, 8 * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT  

        mov ax, [bp + 26d]
        add ax, 6
        mov es:[di], byte ptr 's'
        add di, 2
        mov es:[di], byte ptr 'p'
        add di, 4
        call PrintRegisterValue
        mov di, 9 * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT 

        mov ax, ds
        mov es:[di], byte ptr 'd'
        add di, 2
        mov es:[di], byte ptr 's'
        add di, 4
        call PrintRegisterValue
        mov di, 0ah * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT

        mov ax, [bp + 8]        ; popping real es to ax
        mov es:[di], byte ptr 'e'
        add di, 2
        mov es:[di], byte ptr 's'
        add di, 4
        call PrintRegisterValue
        mov di, 0bh * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT 

        mov ax, ss
        mov es:[di], byte ptr 's'
        add di, 2
        mov es:[di], byte ptr 's'
        add di, 4
        call PrintRegisterValue
        mov di, 0ch * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT 

        mov ax, [bp + 30d]       ; previous cs 
        mov es:[di], byte ptr 'c'
        add di, 2
        mov es:[di], byte ptr 's'
        add di, 4
        call PrintRegisterValue
        mov di, 0dh * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT 

        mov ax, [bp + 28d]       ; previous ip 
        mov es:[di], byte ptr 'i'
        add di, 2
        mov es:[di], byte ptr 'p'
        add di, 4
        call PrintRegisterValue
        mov di, 0eh * SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR + COLUMN_LEFT_SHIFT 

        pop bp
        ret
        endp
     
;------------------------------------------------
;Print one hex digit
;Entry: ax with only least 4 bits,
;       es:di - ptr to print
;Exit : es:di increases by 2
;Destr: di
;------------------------------------------------   
PrintHexDigit proc
        cmp ax, 0ah 
        jb AxIsLess10
        add ax, 'A' - 0ah       ; setting ax 'A' - 'F' ascii code
        mov es:[di], al
        add di, 2
        ret
AxIsLess10:
        add ax, '0'             ; setting ax '0' - '9' ascii code
        mov es:[di], al
        add di, 2
        ret
        endp

;------------------------------------------------
;Print register value
;Entry: ax
;       es:di - ptr to print
;Exit : None
;Destr: di, bx
;------------------------------------------------   
PrintRegisterValue proc
        mov bx, ax

        shr ax, 12
        call PrintHexDigit
        mov ax, bx

        shr ax, 8
        and ax, 0Fh
        call PrintHexDigit
        mov ax, bx

        shr ax, 4
        and ax, 0Fh
        call PrintHexDigit
        mov ax, bx
        
        and ax, 0Fh
        call PrintHexDigit
        mov ax, bx

        ret
        endp


;------------------------------------------------
;Prints one line of the frame
;Entry: ds:si - points to the beginning of the 3-chars array (left, middle, right)
;       es:di - points to the beginning of the video ram to print in
;       ah - color
;       bx - width
;       DF flag set. DF = 0 -> left to right. DF = 1 -> reversed.
;Exit : ds:si moves to the end of the array
;       es:di moves on (bx) bytes to the right
;Destr: cx, si, di
;------------------------------------------------   
PrintFrameLine proc
        lodsb   ; loading to al
        stosw   ; pushing left char

        lodsb   ; loading mid char
        mov cx, bx
        sub cx, 2   ; calculate number of cycle iterations
        rep stosw 

        lodsb   
        stosw       ; pushing right char

        ret
        endp


;------------------------------------------------
;Prints full frame
;Entry: ds:si - points to the beginning of the 9-chars array 
;       ah - frame color
;       bx - frame width
;       cx - frame height
;Exit : ds:si moves to the end of the array
;Destr: cx, si, di, es, dx, bx
;------------------------------------------------   
PrintFrame proc
        push es ds

        mov di, 0B800h
        mov es, di
        mov di, cs
        mov ds, di

        SCREEN_WIDTH  				equ 80d
        NUMBER_OF_BYTES_PER_CHAR	equ 2d
        
        mov di, 0   ; left top corner is 0

        mov dx, cx
        call PrintFrameLine      ; prints top line
        mov cx, dx
        
        sub di, bx  ; moving back to the beginning of the line
        sub di, bx  ; twice because of NUMBER_OF_BYTES_PER_CHAR = 2
        add di, SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR ; next line

        sub cx, 2 ; height
InLoop: mov dx, cx
        push si
        call PrintFrameLine
        sub di, bx  ; moving back to the beginning of the line
        sub di, bx  ; twice because of NUMBER_OF_BYTES_PER_CHAR = 2
        add di, SCREEN_WIDTH * NUMBER_OF_BYTES_PER_CHAR ; next line
        pop si
        mov cx, dx
        loop InLoop

        add si, 3   ; moving ds:si to the next 3 chars (bottom line)
        call PrintFrameLine

        pop ds es

        ret
        endp

;------------------------------------------------
;Reads value in decimal from memory ds:si and stores in ax
;Entry: ds:si - pointer to the string beginning
;       es = ds
;Exit : ax - readen value, 
;       ds:si - points to the char after the string
;Destr: ax, bx, cx, dx, di, si
;------------------------------------------------   
MoveDecimalToRegister proc
        mov di, si
        call SkipSpaces

        mov si, di      ; moving to si str ptr after skipping
        mov cx, 20h
        xor ax, ax ; setting al to zero
        mov bx, 10d
ConvertDecimalLoop:
        cmp ds:[si], cl
        je ConvertDecimalLoopEnd

        mul bx  ; ax *= 10d (actually dx:ax)

        xor dx, dx
        mov dl, byte ptr [si] ; moving number to register
        inc si
        sub dl, '0'
        add ax, dx
        jmp ConvertDecimalLoop

ConvertDecimalLoopEnd:
        ret
        endp


;------------------------------------------------
;Reads value in hex from memory ds:si and stores in ax
;Entry: ds:si - pointer to the string beginning
;       es = ds
;Exit : ax - readen value, 
;       si - points to the char after the string
;Destr: ax, cx, dx, di, si 
;------------------------------------------------   
MoveHexToRegister proc
        mov di, si
        call SkipSpaces

        mov si, di      ; moving to si str ptr after skipping

        mov cx, 20h        
        xor ax, ax ;setting al to zero
ConvertHexLoop:
        cmp ds:[si], cl
        je ConvertHexLoopEnd

        shl ax, 4       ; (*= 16)
        call MoveHexByteToRegister
        add ax, dx
        jmp ConvertHexLoop
        
ConvertHexLoopEnd:
        ret
        endp


;------------------------------------------------
;Reads one byte in hex from memory ds:si and stores in ax
;Entry: ds:si - pointer to the string beginning
;Exit : dx - readen byte, 
;       si - moves on one byte (si + 1)
;Destr: dx, si 
;------------------------------------------------   
MoveHexByteToRegister proc
        xor dx, dx
        mov dl, byte ptr [si]
        inc si

        sub dl, 'a'
        jge MoveHexByteToRegisterReturn
        add dl, 'a' - 'A'
        jge MoveHexByteToRegisterReturn
        add dl, 'A' - '0' - 10d
        ; -10d because of ('b' - 'a' = 1, so i add 10 before ret)
        ; however '2' - '0' = 2

        ; can handle some errors here (ex: byte not in range)

MoveHexByteToRegisterReturn:
        add dl, 10d
        ret
        endp


;------------------------------------------------
;Finds len of the string
;Entry: ES:DI - string begin pointer
;       AL    - string end char
;
;Exit: CX - length of the string
;Destr: DI, CX
;------------------------------------------------
StrLen proc
       xor cx, cx
       dec cx           ; setting cx = FFFF
       repne scasb
       not cx
       dec cx
       ret
       endp


;------------------------------------------------
;Skips chars while it's equal to al
;Entry: ES:DI - string begin pointer
;       AL    - char to skip
;
;Exit : DI - pointer after skipping
;Destr: DI, CX
;------------------------------------------------
SkipWhileChar proc
        xor cx, cx
        dec cx          ; setting cx = FFFF
        repe scasb
        dec di
        ret
        endp


;------------------------------------------------
;Skips spaces
;Entry: ES:DI - string begin pointer
;
;Exit : DI - pointer after skipping
;Destr: DI, AL, CX
;------------------------------------------------
SkipSpaces proc
        mov al, 20h
        call SkipWhileChar
        ret
        endp


;------------------------------------------------
;Reads frame params from command line
;Entry: ds:si - string begin ptr
;Exit : ah - frame color
;       bx - frame width
;       cx - frame height
;       ds:si - skips params
;Destr: ax, bx, cx, si
;------------------------------------------------
ReadFrameParams proc
        call MoveDecimalToRegister      ;reading width 
        push ax
        call MoveDecimalToRegister      ;reading height
        mov bx, ax
        call MoveHexToRegister          ;reading color
        shl ax, 8 ; putting from al to ah

        mov cx, bx
        pop bx

        ret
        endp



;------------------------------------------------
;
;Entry: ds:si - string begin ptr
;Exit : ds:dx - beginning of the 9-chars array with style
;       ds:si - ds:si + skips style chars
;Destr: dx, si, di, cx, al
;------------------------------------------------
ReadFrameStyle proc
        mov di, si
        call SkipSpaces
        mov si, di

        xor dx, dx
        mov dl, byte ptr [si]
        inc si
        cmp dl, '*'
        je CustomStyle

        ; not calling MoveDecimalToRegister because 
        ; this func destroys a lot of registers

        sub dx, '1'
        ;assuming that number is '1' <= dl <= '9'

        mov di, dx
        shl dx, 3
        add dx, di      ;dx * 8 + dx

        add dx, offset cs:FrameChars
        ret

CustomStyle:
        mov dx, si
        add si, 9
        ret

        endp

FrameChars db 0c9h, 0cdh, 0bbh, 0bah, 20h, 0bah, 0c8h, 0cdh, 0bch,      \
                 3,    3,    3,    3, 20h,    3,    3,    3,    3

FrameWidth       dw 0
FrameHeight      dw 0
FrameColor       dw 0
FrameStyleOffset dw 0

EOP:

end Start