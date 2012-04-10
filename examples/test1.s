;test some maths in 0x10c

:start	set a, 123
		add a, 123		;a=246
		sub a, 240		;a=6
		mul a, 3		;a=18
		div a, 2		;a=9 
		set b, a
		mul a, b		;a=81
		set c, 1
		shl a, c		;a=81*2=162
		and a, 0x005d	;a=0
		bor a, 0xff00	;a=65,280
		sub a, 1		;a=65279
		div a, 32767	;a=1
		shr a, c		;a=0
		xor a, 3		;a=3
:end	set PC, end
