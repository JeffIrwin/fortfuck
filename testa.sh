#!/bin/bash

inputs=./samples/*/*.inp

frames=( )

outputext=out

exebase="./a.out"
outdir=.
expectedoutdir=./expected-output
use_envpath="true"
use_exitstat="false"
use_stdout="true"
use_localoutdir="true"

#===============================================================================

source ./submodules/bat/test.sh --no-build

