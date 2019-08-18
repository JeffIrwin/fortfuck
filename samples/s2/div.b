>

i = 201
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
>

j = 2
++
<

========================================================================

Divide
k = i / j
m = i mod j

i  j  f  0  k  m
^

[
	-                  decrement i
	>->>>>+            decrement j and increment m

	<<<+<[>-]>[>]<     test if j == 0 by using flag f and subsequent 0
	   ^ ^
	   | |
	   | if j is non zero then clear f
	   set f to 1

	[
		>>+            increment k
		>[-<<<<+>>>>]  move m to j
		<<<-           clear f
	]
	<<
]

now:
	i is 0
	k is dividend
	m is remainder
	j out is j in minus m

========================================================================
#

Write all els from here rightward
.>.>.>.>.>.>
[.>]
