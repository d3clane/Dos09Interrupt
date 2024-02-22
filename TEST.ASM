.model tiny
.code
.286 

org 100h

Start:	
Cycle:	
	in al, 60h
	cmp al, 2h
	je CycleEnd

	in al, 61h      ; blink high bit in 61h port
	or al, 80h
	out 61h, al
	and al, not 80h
	out 61h, al

	mov bx, 1111h
	mov cx, 2222h
	mov dx, 3333h
	mov si, 4444h
	mov di, 5555h
	jmp Cycle

CycleEnd:
        mov ax, 3100h
        mov dx, offset EOP
        shr dx, 4
        inc dx
        int 21h

EOP:

end Start