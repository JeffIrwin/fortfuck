#!/bin/bash

rm samples/*/a.out

# Testing the full fortfuck pipeline takes three stages:
#
#   1. Run fbc to compile brainfuck into assembly
#   2. Run gcc to link the assembly
#   3. Run the compiled and linked brainfuck executable a.out
#

./testfbc.sh || exit
./testgcc.sh || exit
./testa.sh   || exit

