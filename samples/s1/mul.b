>
++++++
>
+++++++++
<

========================================================================

Multiply
k = i * j

i  j  k  tmpj  tmpi
^

[
	-          decrement i
	>
	[->+>+<<]  add j to k and move j to tmpj
	>>
	[-<<+>>]   move tmpj to j
	>+<        increment tmpi
	<<<
]

Move tmpi to i
>>>>[-<<<<+>>>>]<<<<

========================================================================
#

Write all els from here rightward
[.>]
