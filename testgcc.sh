#!/bin/bash

inputs=./samples/*/*.s

frames=( )

outputext=

NOPIE=
if [[ -e "/etc/debian_version" ]]; then
	NOPIE=-no-pie
fi

exebase="gcc ${NOPIE}"
use_stdin="false"
use_envpath="true"

#===============================================================================

source ./submodules/bat/test.sh --no-build --no-diff

