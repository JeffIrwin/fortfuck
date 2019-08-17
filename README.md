# fortfuck
*A brainfuck-to-assembly translator written in Fortran*

This is a brainfuck compiler written in Fortran.  To compile and use it:

## Compile this compiler

    gfortran -o fbc src/fbc.f90 -Wall -Wextra -Wno-tabs
    
or use `build.sh`.

## Compile one of the brainfuck sample programs

    ./fbc samples/s0/add.b

See also `test.sh`.

## Assemble the intermediate output

Debian systems (e.g. Ubuntu) need the `-no-pie` option.  Other Linux systems do not.

    gcc -o add samples/s0/add.s -no-pie

## Run the compiled brainfuck program

    echo "12" | ./add

The expected output is "12c", as the program adds the ASCII codes of the characters: `1` (49) + `2` (50) = `c` (99).

## Platforms

This build toolchain has been tested on Ubuntu 18.04.2 LTS (Bionic Beaver) with gfortran GNU Fortran (Ubuntu 7.4.0-1ubuntu1\~18.04.1) 7.4.0 and gcc (Ubuntu 7.4.0-1ubuntu1\~18.04.1) 7.4.0.

Other platforms and Fortran compilers may work.  Other assemblers that support AT&T syntax for x86-64 may also work.

