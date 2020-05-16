#!/bin/bash

rm samples/*/a.out

./testfbc.sh || exit
./testgcc.sh || exit
./testa.sh   || exit

