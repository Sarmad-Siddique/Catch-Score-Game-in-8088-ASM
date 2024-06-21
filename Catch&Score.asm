[org 0x0100]

	jmp start

oldkbisr:	dd 0				; storing old keyboard isr
oldtmisr:	dd 0				; storing old timer isr


lcounter: 	dw 0				; loop counter, rn using it to make diamond shape (might need for other functions later)
buckloc:	dw 70				; the x-pos of bucket   (~10 to ~290)
tcounter: 	dw 0				; timer counter, to store number of timer ticks (0 to 18 and then resets to 0)

timestr: 	db 'TIME: '
scorestr:	db 'SCORE: '

time:		dw 120			; current time left
score:	dw 0				; current score

gover:	dw 1				; status of game (1 for ongoing, 0 for over)

rcoord:	TIMES 360 dw 0		; 120 length array to store random coordinates for gems to drop at
rgem:		TIMES 360 dw 0		; 120 length array to radomly pick which gem (color) to display

rcounter:	dw 0				; ticks since last gem displayed, change range in timer isr to make faster or slower
rand:		dw 0 				; offset for the random arry of coords and gems

PRN:		dw 0				; seed for the rndom generators

msg1:		db 'Press ENTER to Start Game'
msg2:		db 'GAME OVER!'
msg3:		db 'Your final score was '

mytmisr:
	push ax
	push ds
	push es

	cld
	push cs
	pop ds

	cmp word[gover], 0
	jz sk

	inc word[tcounter]
	inc word[rcounter]

	; Displaying random gems in random coordinates at certain intervals

	cmp word[rcounter], 6
	jnz nxt

	call gengem

	add word[rand], 2

	mov word[rcounter], 0
	

	; timer portion of the isr
	
nxt:	cmp word[tcounter], 18
	jnz sk
	
	mov word[tcounter], 0
	sub word[time], 1

	call distime

	cmp word[time], 0
	jnz sk

	mov word[gover], 0
sk:
	mov al, 0x20
	out 0x20, al

	pop es
	pop ds
	pop ax

	iret

gengem:

	push ax
	push bx
	push di

	mov di, [rand]
	
	mov ax, [rgem + di]

	cmp ax, 4
	jge nxtcmp1

	mov bx, 0x2F
	push bx			; color

	jmp gem

nxtcmp1:
	cmp ax, 8
	jge nxtcmp2

	mov bx, 0x0
	push bx			; color

	jmp gem

nxtcmp2:
	cmp ax, 12
	jge nxtcmp3

	mov bx, 0x27
	push bx			; color

	jmp gem

nxtcmp3:
	mov bx, 0x0E
	push bx			; color
	
gem:	

	mov bx, 20
	push bx			; y-coordinate

	mov ax, [rcoord + di]
	shl ax, 2
	add ax, 20
	
	push ax			; x-coordinate

	call drawdia

	pop di
	pop bx
	pop ax

	ret	

mykbisr:
	push ax
	push es
	push ds

	cld
	push cs
	pop ds

	cmp word[gover], 0
	jz nomatch
	

	xor ax, ax

	in al, 0x60

	cmp al, 0x4D
	jne nextcmp

	cmp word[buckloc], 290
	jge exit

	call clearbucket

	add word[buckloc], 20
	
	mov ax, 4
	push ax
	call disbuck

	jmp exit

nextcmp:
	cmp al, 0x4B
	jne nomatch

	cmp word[buckloc], 20
	jb exit

	call clearbucket

	sub word[buckloc], 20
	
	mov ax, 4
	push ax
	call disbuck

	jmp exit

nomatch:
	pop ds
	pop es
	pop ax
	jmp far [cs:oldkbisr] 			; call the original ISR
	
exit:
	mov al, 0x20
	out 0x20, al 				; send EOI to PIC

	pop ds
	pop es
	pop ax
	
	iret 						; return from interrupt

disST:
	push dx
	push bx
	push ax
	push cx
	push si

	; Setting Cursor Position for Score

	mov dl, 0
	mov dh, 0
	mov bh, 0
	mov ah, 02h
	int 10h

	; Printing out "score: "

	xor si, si
	mov cx, 7

p1:	
	mov al, [scorestr + si]
	mov bl, 0ch
	mov bh, 0
	mov ah, 0Eh
	int 10h
	
	inc si
	loop p1

	; Display the score

	mov cx, [score]
	push cx
	call printnum


	; Setting Cursor Position for time

	mov dl, 30
	mov dh, 0
	mov bh, 0
	mov ah, 02h
	int 10h

	; Printing out "time: "

	xor si, si
	mov cx, 6

p2:	
	mov al, [timestr + si]
	mov bl, 0ch
	mov bh, 0
	mov ah, 0Eh
	int 10h
	
	inc si
	loop p2

	; display the time left

	mov cx, [time]
	push cx
	call printnum

	pop si
	pop cx
	pop ax
	pop bx
	pop dx

	ret

distime:
	push cx
	push dx
	push bx
	push ax
	
	
	mov dl, 36
	mov dh, 0
	mov bh, 0
	mov ah, 02h
	int 10h
	

	mov cx, [time]
	push cx

	cmp cx, 100
	jge pr

	mov bh, 0
	mov bl, 0Eh					; yellow on black bg
	mov ah, 0Eh
	mov al, '0'
	int 10h

	cmp cx, 10
	jge pr

	mov bh, 0
	mov bl, 0Eh					; yellow on black bg
	mov ah, 0Eh
	mov al, '0'
	int 10h	

pr:
	call printnum

	pop ax
	pop bx
	pop dx
	pop cx

	ret

disScore:
	push cx
	push dx
	push bx
	push ax
	
	
	mov dl, 7
	mov dh, 0
	mov bh, 0
	mov ah, 02h
	int 10h
	

	mov cx, [score]
	push cx
	call printnum

	pop ax
	pop bx
	pop dx
	pop cx

	ret

printnum:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx

	mov ax, [bp + 4]
	mov bx, 10
	mov cx, 0

nextdigit:
	mov dx, 0
	div bx
	add dl, 0x30
	push dx
	inc cx
	cmp ax, 0
	jnz nextdigit

nextpos:
	pop ax
	mov bh, 0
	mov bl, 0Eh				; yellow text with black blackground
	mov ah, 0Eh
	int 10h
	loop nextpos

	pop dx
	pop cx
	pop bx
	pop ax
	pop bp

	ret 2	
	
bg:	
	push bp
	mov bp, sp
	push ax
	push cx
	push bx
	push dx
	push es
	push ds
	push si
	push di
	pushf

	mov ax, 0xA000
	mov es, ax
	
	
	xor si, si
	xor di, di

	mov al, 0

h1:	mov cx, 320
	rep stosb
	
	inc si
	cmp si, 8
	jl h1

	
	mov al, 0x4F

h2:	mov cx, 320
	rep stosb
	
	inc si
	cmp si, 200
	jl h2


	popf
	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop bx
	pop cx
	pop ax
	pop bp

	ret 

drawdia:
	push bp
	mov bp, sp
	push ax
	push cx
	push bx
	push dx
	push es
	push ds
	push si
	push di
	pushf

	mov ah, 0x0C 			; put pixel 
	mov al, [bp + 8]			; pixel color
	xor bx, bx 				; page number 0
	
	mov cx, [bp + 4] 			; first x position
	mov dx, [bp + 6]			; first y position

	mov si, [bp + 4]			
	inc si				; max length of x-axis (we increment this in our loop

	mov di, [bp + 6]			
	add di, 10				; max length of y-axis (initial y-pos + height of diamond)

	mov word[lcounter], 0
l2:						; first loop is used for making the upper triangle (using lcounter variable)
	int 10h
	inc cx				; incrementing x axis
	
	cmp cx, si
	jne l2

	inc word[lcounter]		; incrementing lcounter
	sub cx, [lcounter]
	sub cx, [lcounter]		; subtracting lcounter from cx(x-coordinate) twice

	inc dx				; incrementing y axis
	inc si

	cmp dx, di
	jne l2

	
	add di, 8
	sub si, 3
	add cx, 2
	sub word[lcounter], 2
l3:						; 2nd loop used for making the lower triangle (top to bottom)
	int 10h
	inc cx				; incrementing x axis
	
	cmp cx, si
	jbe l3

	
	mov cx, [bp + 4]
	dec word[lcounter]
	sub cx, [lcounter]
	

	inc dx				; incrementing y axis
	dec si
	
	cmp dx, di
	jbe l3


	popf
	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop bx
	pop cx
	pop ax
	pop bp

	ret 6

scrolldown:
	push ax
	push cx
	push di
	push si
	push es

	mov ax, 0xA000
	mov es, ax
	mov ds, ax

	mov di, 63999
	mov si, 63039 				; scrolls by 3  (52719 for more difficulty)

	mov cx, 59840				; doesnt scroll the first 8 rows    (59520)
	
	std

	rep movsb 

	cld
	
	push cs
	pop ds

	pop es
	pop si
	pop di
	pop cx
	pop ax
	
	ret

disbuck:
	push bp
	mov bp, sp

	push es
	push ax
	push di
	push dx
	push cx
	push bx
	push ds

	;push cs
	;pop ds
	
	mov ax, 0xA000
	mov es, ax

	mov di, [buckloc]
	add di, 54400		; adding 170 rows (so the bucket will start at 170th row always)

	mov bx, di


	mov al, [bp + 4]
	mov dx, 2			; thickness of the bucket
	cld

b3:
	mov cx, 20
	
b1:	
	stosb
	add di, 319
	
	loop b1
	

	mov cx, 20
	rep stosb


	mov cx, 21
b2:	
	stosb
	sub di, 321
	loop b2

	mov di, bx
	inc di

	dec dx
	jnz b3

	mov di, bx

	mov cx, 21
b4:	
	stosb
	add di, 320
	loop b4

	mov di, bx
	add di, 20

	mov cx, 21
b5:	
	stosb
	add di, 318
	loop b5

	mov di, bx
	add di, 3200
	
	mov cx, 10
b6:	
	stosb
	add di, 320
	loop b6

	mov cx, 10
b7:	
	stosb
	sub di, 320
	loop b7

	mov cx, 10
b8:	
	stosb
	sub di, 322
	loop b8

	mov cx, 10
b9:	
	stosb
	add di, 318
	loop b9
	
	pop ds
	pop bx
	pop cx
	pop dx
	pop di
	pop ax
	pop es
	pop bp

	ret 2

clearbuck:					; in regards to scrolling
	push es
	push ax
	push di
	push dx
	push cx
	push ds

	;push cs
	;pop ds
	
	mov ax, 0xA000
	mov es, ax

	mov di, [buckloc]
	add di, 61760

	mov dx, 3

	mov al, 0x4F
	cld
c1:	
	mov cx, 23
	rep stosb

	add di, 297
	dec dx
	jnz c1	
	

	pop ds
	pop cx
	pop dx
	pop di
	pop ax
	pop es

	ret 

clearbucket:					; in regards to movement
	push es
	push ax
	push di
	push dx
	push cx
	push ds

	;push cs
	;pop ds
	
	mov ax, 0xA000
	mov es, ax

	mov di, [buckloc]
	add di, 54080

	mov dx, 29

	mov al, 0x4F
	cld
c2:	
	mov cx, 25
	rep stosb

	add di, 295
	sub dx, 1
	jnz c2	

	pop ds
	pop cx
	pop dx
	pop di
	pop ax
	pop es

	ret 

checkforgem:

	push ax
	push bx
	push cx
	push dx
	push es

	mov ax, 0xA000
	mov es, ax

	mov di, [buckloc]
	add di, 52485

	mov al, [es:di]

	cmp al, 0x0
	jz ovr

	cmp al, 0x0E
	jz yel

	cmp al, 0x27
	jz red

	cmp al, 0x2F
	jz grn

	
	add di, 10

	mov al, [es:di]

	cmp al, 0x0
	jz ovr

	cmp al, 0x0E
	jz yel

	cmp al, 0x27
	jz red

	cmp al, 0x2F
	jz grn

	jmp skp

ovr:	
	mov word[gover], 0
	jmp skp

red:	
	add word[cs:score], 15
	jmp clg

yel:	
	add word[cs:score], 10
	jmp clg

grn:	
	add word[cs:score], 5

clg:	
	call disScore
	call cleargem
	
skp:

	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	
	ret


cleargem:
	push es
	push ax
	push di
	push dx
	push cx
	push ds

	;push cs
	;pop ds
	
	mov ax, 0xA000
	mov es, ax

	mov di, [buckloc]
	add di, 46065

	mov dx, 40

	mov al, 0x4F
	cld
c4:	
	mov cx, 40
	rep stosb

	add di, 280
	sub dx, 1
	jnz c4

	mov ax, 4
	push ax
	call disbuck

	pop ds
	pop cx
	pop dx
	pop di
	pop ax
	pop es

	ret 

CalcNew:

	mov ax, 25173
	mul word[PRN]
	add ax, 13849

	mov word[PRN], ax
	shr ax, 5

	ret

randgen:

	push ax
	push bx
	push cx
	push dx
	push di

	mov ah, 00h
	int 1Ah

	mov word[PRN], dx
	mov cx, 360
	xor di, di

k1:	
	call CalcNew

	xor dx, dx
	mov bx, 70
	div bx

	mov word[rcoord + di], dx
	add di, 2

	loop k1

	
	mov ah, 00h
	int 1Ah

	mov word[PRN], dx
	mov cx, 360
	xor di, di

k2:	
	call CalcNew

	xor dx, dx
	mov bx, 16
	div bx

	mov word[rgem + di], dx
	add di, 2

	loop k2

	pop di
	pop dx
	pop cx
	pop bx
	pop ax

	ret

startscr:
	push ax
	push es
	push si
	push di
	push bx
	push dx
	push cx


	mov ax, 0xA000
	mov es, ax
	
	
	xor si, si
	xor di, di

	mov al, 0x4F

o1:	mov cx, 320
	rep stosb
	
	inc si
	cmp si, 200
	jl o1


	; Setting Cursor Position for Message

	mov dl, 8
	mov dh, 12
	mov bh, 0
	mov ah, 02h
	int 10h

	; Printing out 

	xor si, si
	mov cx, 25

o2:	
	mov al, [msg1 + si]
	mov bl, 0ch
	mov bh, 0
	mov ah, 0Eh
	int 10h
	
	inc si
	loop o2

	
	pop cx
	pop dx
	pop bx
	pop di
	pop si
	pop es
	pop ax

	ret

endscr:
	push ax
	push es
	push si
	push di
	push bx
	push dx
	push cx

	call bg

	; Setting Cursor Position for Message 2

	mov dl, 15
	mov dh, 10
	mov bh, 0
	mov ah, 02h
	int 10h

	; Printing out 

	xor si, si
	mov cx, 10

o3:	
	mov al, [msg2 + si]
	mov bl, 0ch
	mov bh, 0
	mov ah, 0Eh
	int 10h
	
	inc si
	loop o3


	; Setting Cursor Position for Message 3

	mov dl, 8
	mov dh, 14
	mov bh, 0
	mov ah, 02h
	int 10h

	; Printing out 

	xor si, si
	mov cx, 21

o4:	
	mov al, [msg3 + si]
	mov bl, 0ch
	mov bh, 0
	mov ah, 0Eh
	int 10h
	
	inc si
	loop o4

	mov ax, [score]
	push ax
	call printnum

	pop cx
	pop dx
	pop bx
	pop di
	pop si
	pop es
	pop ax

	ret

delay:
	push cx

	mov cx, 0x0f
	
d1:
	push cx
	mov cx, 0xffff
d2:
	loop d2
	
	pop cx
	loop d1

	pop cx
	ret

start:
	
	; initializing the arrays for random coordinates and random gems
	call randgen


	; Setting Video Mode (Graphics mode 13h)
	mov ah, 0
	mov al, 13h
	int 0x10

	; Start Screen and wait for ENTER key press
	call startscr

w2:	
	mov ah, 00h
	int 16h

	cmp ah, 0x1C						; scan code of ENTER
	jne w2


	; Hooking int 9 (related to keyboard)

	xor ax, ax
	mov es, ax 					; point es to IVT base
	mov ax, [es:9*4]
	mov [oldkbisr], ax 			; save offset of old routine
	mov ax, [es:9*4+2]
	mov [oldkbisr+2], ax 			; save segment of old routine

	cli 						; disable interrupts
	mov word [es:9*4], mykbisr 		; store offset at n*4
	mov [es:9*4+2], cs			; store segment at n*4+2
	sti

	; Hooking int 8 (related to timer)

	xor ax, ax
	mov es, ax 					; point es to IVT base
	mov ax, [es:8*4]
	mov [oldtmisr], ax 			; save offset of old routine
	mov ax, [es:8*4+2]
	mov [oldtmisr+2], ax 			; save segment of old routine

	cli 						; disable interrupts
	mov word [es:8*4], mytmisr 		; store offset at n*4
	mov [es:8*4+2], cs			; store segment at n*4+2
	sti


	; Background 
	call bg

	; Score and Timer
	call disST

	mov ax, 4
	push ax
	call disbuck

	
w1:
	call scrolldown

	mov ax, 4
	push ax
	call disbuck
	
	call clearbuck

	call checkforgem
	
	cmp word[gover], 0
	jnz w1

	call endscr

w3:	
	mov ah, 00h
	int 16h

	cmp ah, 0x1			; scan code of ESC
	jne w3
	
end:
	mov ax, 0x0003 		; 80x25 text mode
	int 0x10 			; bios video services

	mov ax, 0x4c00 		; terminate program
	int 0x21