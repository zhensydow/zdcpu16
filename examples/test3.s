;convert a 16-bit value into a string
;examples of simple functions and print

:start	set a, 34348
	jsr to_string
	set z, x		;[x] is the string, but needs to be [z]
	set x, 5		;set x/y positions
	set y, 5
	jsr print		;print the string
:end	sub PC, 1		;terminate program

;pass the value required in register a
;[x] is the new string when it returns
;destroys the a, b, c, i and x registers

:to_string
	set x, strend
:loop	set b, a
	ife b, 0		;b=0? then we have finished
	set PC, POP
	div b, 10
	set i, b		;store this value somewhere
	mul b, 10		;b = b * 10, we have now 'lost' the last digit
	set c, a
	sub c, b		;c = b - a, that is, the value of the lost digit
	add c, zero		;add to char for '0'
	sub x, 1		;move backwards along the string
	set [x], [c]	;set to string
	set a, i		;restore the value / 10
	set PC, loop
		
:string dat "     "
:strend	dat 0
:zero	dat "0123456789"

;print a string. pass x and y as co-ords to print to, [z] as the string
;string must be 0 terminated
;destroys the x, y, and z registers

:print
	mul y, 32
	add x, y
	add x, 0x8000	;0x800 is start of video RAM
:ploop
	ife [z], 0		;end of string?
	set PC, POP		;then return
	set [x], [z]
	add z, 1
	add x, 1
	set PC, ploop
