#!/bin/bash

inputs=./samples/*/*.b

frames=( )

outputext=s

exebase=fbc
outdir=.
use_stdin="false"
use_localoutdir="true"

#===============================================================================

source ./submodules/bat/test.sh --no-diff

