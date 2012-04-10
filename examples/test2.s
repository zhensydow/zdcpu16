;Video memory goes from 0x8000 to 0x8400

;draw a pretty pattern
;the pattern is (x^2 + y^2) & 3

;we need 2 loops, for y and x

		set y, 0
		set i, chars
:loopy	set x, 0
:loopx	set b, x
		mul b, b			;b = x^2
		set c, y
		mul c, c			;c = y^2
		add b, c			;b = x^2 + y^2
		and b, 3			;b = (x^2 + y^2) & 3
		add i, b
		set z, [i]			;short of registers, so we add and then reset
		sub i, b
		jsr plot
		add x, 1
		ifn x, 32
		set PC, loopx		;inner loop complete
		add y, 1
		ifn y, 16
		set PC, loopy
:end	sub PC, 1

:chars	dat " .oO"

;a function to 'plot' a character on the screen
;pass the co-rds in the x and y registers, and what to plot in z
;destroys the a register
;call with set PC, plot

:plot	set a, y
		mul a, 32
		add a, x
		add a, 0x8000
		set [a], z
		set PC, POP