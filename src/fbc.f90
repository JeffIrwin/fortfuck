
!=======================================================================

! This is a brainfuck compiler written in Fortran.  See README.md for
! build and usage instructions.

! The 6 simple brainfuck commands are encoded alphabetically as the
! following assembly subroutines:
!
!     .  d  (dot)
!     ,  c  (comma)
!     -  m  (minus)
!     +  p  (plus)
!     <  l  (less than)
!     >  g  (greater than)
!
! The other 2 brainfuck commands (loops []) are assembled with assembly
! labels startloop{i} and endloop{i}, where {i} is an index that is set
! by the compiler according to the number and nestedness of loops.
!
! For input/output (, and .), the C functions getc and putchar are used.
! The assembly code output by this compiler uses AT&T syntax.

!=======================================================================

module m

	use iso_fortran_env

	implicit none

	character, parameter :: lf = char(10)

	character, parameter :: &
			dot     = '.',      &
			comma   = ',',      &
			minus   = '-',      &
			plus    = '+',      &
			less    = '<',      &
			greater = '>',      &
			bra     = '[',      &
			ket     = ']'

	character(len = *), parameter :: this = 'fbc'
	character(len = *), parameter :: &
			dotsub     = "d",            &
			commasub   = "c",            &
			minussub   = "m",            &
			plussub    = "p",            &
			lesssub    = "l",            &
			greatersub = "g"

	character(len = *), parameter ::           &
			dotasm     =              &
			"mov (%rbx), %rdi"//lf//  &
			"call putchar",           &
			commaasm   =              &
			"mov stdin, %rdi"//lf//   &
			"call getc"      //lf//   &
			"mov %rax, (%rbx)",       &
			minusasm   =              &
			"decq (%rbx)",            &
			plusasm    =              &
			"incq (%rbx)",            &
			lessasm    =              &
			"add $0x8, %rbx",         &
			greaterasm =              &
			"sub $0x8, %rbx"//lf//    &
			"cmp %rsp, %rbx"//lf//    &  ! Are we moving into the stack, which needs to be used for other things (e.g. subroutine calls, getc, putchar, ...) ?
			"jle padstack"

	integer, save :: debug = 0

	integer, parameter ::       &
			ERR_PARSEARGS     =   -1, &
			ERR_COMPILE       =   -2, &
			ERR_NO_INPUT      =   -3, &
			ERR_FILE_NO_EXIST =   -4, &
			ERR_WRITEHEADER   =   -5, &
			ERR_WRITETAIL     =   -6, &
			ERR_NEWSTACK      =   -7, &
			ERR_POP           =   -8, &
			ERR_PUSH          =   -9, &
			ERR_UNMATCHED_KET =  -10, &
			ERR_UNMATCHED_BRA =  -11, &
			WARN_UNKNOWN_ARG  =    1, &
			SUCCESS           =    0

	type brainfuck
		character(len = :), allocatable :: ifile, ofile
		logical :: inline
	end type brainfuck

	! To do?  Make a file object including name and unit number,
	! encapsulate ifile and ofile.

	type stack
		integer, allocatable :: s(:)
		integer :: n, i
	end type stack

contains

!=======================================================================

subroutine logger(io, str)

	character*(*), optional :: str

	integer :: io

	! To do?  Store these messages in string arrays.

	if (io == ERR_PARSEARGS) then
		write(*,*) this//': error: cannot parse command line arguments'

	else if (io == ERR_COMPILE) then
		write(*,*) this//': error: cannot compile brainfuck program'

	else if (io == ERR_NO_INPUT) then
		write(*,*) this//': error: no input files'

	else if (io == ERR_FILE_NO_EXIST) then
		write(*,*) this//': error: "'//str//'": no such file or directory'

	else if (io == ERR_NEWSTACK) then
		write(*,*) this//': error: cannot allocate new stack'

	else if (io == ERR_POP) then
		write(*,*) this//': error: cannot pop value from empty stack'

	else if (io == ERR_PUSH) then
		write(*,*) this//': error: cannot reallocate while pushing new value to stack'

	else if (io == ERR_UNMATCHED_KET) then
		write(*,*) this//': error: unmatched right bracket "'//ket//'"'

	else if (io == ERR_UNMATCHED_BRA) then
		write(*,*) this//': error: unmatched left bracket "'//bra//'"'

	else if (io == WARN_UNKNOWN_ARG) then
		write(*,*) this//': warning: unrecognized command line option "'//str//'"'

	else if (io < 0) then
		write(*,*) this//': error: undefined error'

	else if (io > 0) then
		write(*,*) this//': warning: undefined warning'

	end if

	write(*,*)

end subroutine logger

!=======================================================================

integer function parseargs(bf)

	character :: dummy
	character(len = :), allocatable :: argv

	integer :: argc, i, io, length

	logical, save :: ifiledefined = .false.

	type(brainfuck) :: bf

	if (debug > 0) print *, 'starting parseargs...'

	parseargs = ERR_PARSEARGS
	bf%inline = .true.
	dummy = ''
	argc = command_argument_count()

	if (debug > 0) print *, 'argc = ', argc

	do i = 1, argc

		call get_command_argument(i, dummy, length, io)
		if (debug > 2) print *, 'length = ', length

		argv = repeat(' ', length)
		call get_command_argument(i, argv, length, io)
		if (debug > 1) print *, 'argv = ', argv

		! TODO:  add optimization options, output name option, verbosity
		! option, compile/run time check options, help option, inline
		! option, ...

		if (.not. ifiledefined) then
			ifiledefined = .true.
			bf%ifile = argv
		else
			call logger(WARN_UNKNOWN_ARG, argv)
		end if

	end do

	if (.not. ifiledefined) then
		parseargs = ERR_NO_INPUT
		call logger(parseargs)
		return
	end if

	parseargs = SUCCESS

end function parseargs

!=======================================================================

integer function compile(bf)

	character :: c

	integer :: i, io, fi, fo, ibra, iket

	logical :: fexist
	logical, save :: eof = .false.

	type(brainfuck) :: bf

	type(stack) :: brackets

	if (debug > 0) print *, 'starting compile...'

	compile = ERR_COMPILE
	ibra = -1
	io = newstack(brackets)
	if (io /= 0) return

	inquire(file = bf%ifile, exist = fexist)
	if (.not. fexist) then
		compile = ERR_FILE_NO_EXIST
		call logger(compile, bf%ifile)
		return
	end if

	bf%ofile = 'a.s'
	i = scan(bf%ifile, '.', .true.)
	if (i /= 0) then
		bf%ofile = bf%ifile(1: i)//'s'
	else
		bf%ofile = bf%ifile//'.s'
	end if

	write(*,*) 'Compiling "'//bf%ofile//'" from "'//bf%ifile//'"...'

	open(file = bf%ifile, newunit = fi, iostat = io, status = 'old')
	if (io /= 0) then
		call logger(compile)
		return
	end if

	open(file = bf%ofile, newunit = fo, iostat = io)
	if (io /= 0) then
		call logger(compile)
		return
	end if

	io = writeheader(fo)
	if (io /= 0) then
		call logger(compile)
		return
	end if

	do while (.not. eof)
		read(fi, '(a)', iostat = io, advance = 'no') c
		eof = io == iostat_end
		if (.not. eof) then

			if (debug > 2) print *, c

			! To do?  Track source line number and output error diagnostics.
			! The only possible errors are unmatched brackets, which can't be
			! isolated to a single character or line, especially in brainfuck
			! which doesn't care about source lines.

			if      (c == dot    ) then

				if (bf%inline) then
					write(fo, '(a)') dotasm
				else
					write(fo, '(a)') "call "//dotsub
				end if

			else if (c == comma  ) then

				if (bf%inline) then
					write(fo, '(a)') commaasm
				else
					write(fo, '(a)') "call "//commasub
				end if

			else if (c == minus  ) then

				if (bf%inline) then
					write(fo, '(a)') minusasm
				else
					write(fo, '(a)') "call "//minussub
				end if

			else if (c == plus   ) then

				if (bf%inline) then
					write(fo, '(a)') plusasm
				else
					write(fo, '(a)') "call "//plussub
				end if

			else if (c == less   ) then

				if (bf%inline) then
					write(fo, '(a)') lessasm
				else
					write(fo, '(a)') "call "//lesssub
				end if

			else if (c == greater) then

				!! This one is tricker to inline since it includes a jump to
				!! padstack.  Padstack will need to be inlined too (with some
				!! mangling to avoid duplicate padstacks).
				!if (bf%inline) then
				!	write(fo, '(a)') greaterasm
				!else
					write(fo, '(a)') "call "//greatersub
				!end if

			else if (c == bra    ) then

				ibra = ibra + 1
				io = push(brackets, ibra)
				if (io /= 0) return

				write(fo, '(a)') "cmp $0x0, (%rbx)"
				write(fo, '(a)') "je endloop"//itoa(ibra)
				write(fo, '(a)') "startloop" //itoa(ibra)//":"

			else if (c == ket    ) then

				if (brackets%i < 1) then
					compile = ERR_UNMATCHED_KET
					call logger(compile)
					return
				end if

				io = pop(brackets, iket)
				if (io /= 0) return

				write(fo, '(a)') "cmp $0x0, (%rbx)"
				write(fo, '(a)') "jne startloop"//itoa(iket)
				write(fo, '(a)') "endloop"//itoa(iket)//":"

			else
				! Do nothing for non-brainfuck characters.
			end if

		end if
	end do

	if (brackets%i > 0) then
		compile = ERR_UNMATCHED_BRA
		call logger(compile)
		return
	end if

	io = writetail(fo)
	if (io /= 0) then
		call logger(compile)
		return
	end if

	close(fo)
	compile = SUCCESS

end function compile

!=======================================================================

integer function newstack(st)

	integer :: io
	type(stack) :: st

	newstack = ERR_NEWSTACK
	st%n = 1
	st%i = 0
	allocate(st%s(st%n), stat = io)
	if (io /= 0) then
		call logger(newstack)
		return
	end if
	st%s = 0
	newstack = SUCCESS

end function newstack

!=======================================================================

integer function pop(st, poppedval)

	integer :: poppedval
	type(stack) :: st

	pop = ERR_POP
	if (st%i < 1) then
		call logger(pop)
		return
	end if

	poppedval = st%s(st%i)
	st%i = st%i - 1
	pop = SUCCESS

end function pop

!=======================================================================

integer function push(st, pushedval)

	integer :: io, pushedval, ntmp
	integer, allocatable :: tmp(:)
	type(stack) :: st

	push = ERR_PUSH

	st%i = st%i + 1
	!print *, 'st%i = ', st%i
	!print *, 'st%n = ', st%n
	if (st%i > st%n) then
		!print *, 'realloc'

		ntmp = int(1.1 * st%i + 1)

		allocate(tmp(ntmp), stat = io)
		if (io /= 0) then
			call logger(push)
			return
		end if

		tmp(1: st%n) = st%s(1: st%n)
		deallocate(st%s)
		call move_alloc(tmp, st%s)
		st%n = ntmp
	end if
	!print *, 'pushedval = ', pushedval

	st%s(st%i) = pushedval
	push = SUCCESS

end function push

!=======================================================================

function itoa(i) result(a)

	character :: tmp*64
	character(len = :), allocatable :: a

	integer :: i

	write(tmp, '(i0)') i
	a = trim(tmp)

end function itoa

!=======================================================================

integer function writeheader(f)

	integer :: f

	writeheader = ERR_WRITEHEADER

	write(f, '(a)') ".global main"
	write(f, '(a)') ".text"
	write(f, '(a)') "main:"

	! TODO: encode register names, target/source order syntax, etc. as
	! variables for extension to other architectures and direct output as
	! object file.

	! Setup a local stack frame
	write(f, '(a)') "push %rbp"
	write(f, '(a)') "push %rbx"
	write(f, '(a)') "mov %rsp, %rbp"

	! Initialize rbx as brainfuck data pointer.
	write(f, '(a)') "mov %rsp, %rbx"

	writeheader = SUCCESS

end function writeheader

!=======================================================================

integer function writetail(f)

	integer :: f

	writetail = ERR_WRITETAIL

	! Output a linefeed ($0xa in ASCII).  This is not standard, but it
	! helps readability.

	!! Add carriage return for Windows line endings
	!write(f, '(a)') "mov $0xd, %rdi"
	!write(f, '(a)') "call putchar"

	write(f, '(a)') "mov $0xa, %rdi"
	write(f, '(a)') "call putchar"

	! Restore stack frame
	write(f, '(a)') "mov %rbp, %rsp"
	write(f, '(a)') "pop %rbx"
	write(f, '(a)') "pop %rbp"

	write(f, '(a)') "ret"

	! TODO:  return if inlining.  Need to fix padstack.

	! Print the current data byte
	write(f, '(a)') dotsub//":"
	write(f, '(a)') dotasm
	write(f, '(a)') "ret"

	! Get a byte of data from standard input
	write(f, '(a)') commasub//":"
	write(f, '(a)') commaasm
	write(f, '(a)') "ret"

	! Decrement data value
	write(f, '(a)') minussub//":"
	write(f, '(a)') minusasm
	write(f, '(a)') "ret"

	! Increment data value
	write(f, '(a)') plussub//":"
	write(f, '(a)') plusasm
	write(f, '(a)') "ret"

	! Move the data pointer to the left
	write(f, '(a)') lesssub//":"
	write(f, '(a)') lessasm
	write(f, '(a)') "ret"

	! Move the data pointer to the right
	write(f, '(a)') greatersub//":"
	write(f, '(a)') greaterasm
	write(f, '(a)') "ret"

	write(f, '(a)') "padstack:"

	! Backup the stack value.
	write(f, '(a)') "mov (%rsp), %rcx"

	! Move the new stack pointer and decrement.
	write(f, '(a)') "mov %rbx, %rsp"
	write(f, '(a)') "sub $0x8, %rsp"

	! Restore the backed up value.
	write(f, '(a)') "mov %rcx, (%rsp)"

	! Initialize brainfuck data value to zero (stack may not be
	! initialized).
	write(f, '(a)') "movq $0x0, (%rbx)"

	write(f, '(a)') "ret"

	writetail = SUCCESS

end function writetail

!=======================================================================

end module m

!=======================================================================

program main

	use m

	implicit none

	integer :: io

	type(brainfuck) :: bf

	if (debug > 0) then
		write(*,*)
		write(*,*) 'Starting brainfuck compiler...'
		write(*,*)
	end if

	io = parseargs(bf)
	if (io /= 0) call exit(io)
	io = compile(bf)
	if (io /= 0) call exit(io)

	if (debug > 0) then
		write(*,*) 'Done!'
		write(*,*)
	end if

	call exit(SUCCESS)

end program main

!=======================================================================

