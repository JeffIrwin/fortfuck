#!/bin/bash

./build.sh

echo "Running tests..."

nfail=0
ntotal=0

NOPIE=
if [[ -e "/etc/debian_version" ]]; then
	NOPIE=-no-pie
fi

ntotal=$((ntotal + 1))
rm samples/s0/add.s
./fbc samples/s0/add.b
rm a.out
gcc samples/s0/add.s ${NOPIE}
rm samples/s0/output.txt
./a.out < samples/s0/input.txt > samples/s0/output.txt
diff samples/s0/expected-output.txt samples/s0/output.txt > samples/s0/diff.txt
if [[ "$?" == "1" ]]; then
	nfail=$((nfail + 1))
	echo "test.sh:  error:  diff for sample s0:"
	cat samples/s0/diff.txt
fi

ntotal=$((ntotal + 1))
rm samples/s1/mul.s
./fbc samples/s1/mul.b
rm a.out
gcc samples/s1/mul.s ${NOPIE}
rm samples/s1/output.txt
./a.out < samples/s1/input.txt > samples/s1/output.txt
diff samples/s1/expected-output.txt samples/s1/output.txt > samples/s1/diff.txt
if [[ "$?" == "1" ]]; then
	nfail=$((nfail + 1))
	echo "test.sh:  error:  diff for sample s1:"
	cat samples/s1/diff.txt
fi

ntotal=$((ntotal + 1))
rm samples/s2/div.s
./fbc samples/s2/div.b
rm a.out
gcc samples/s2/div.s ${NOPIE}
rm samples/s2/output.txt
./a.out < samples/s2/input.txt > samples/s2/output.txt
diff samples/s2/expected-output.txt samples/s2/output.txt > samples/s2/diff.txt
if [[ "$?" == "1" ]]; then
	nfail=$((nfail + 1))
	echo "test.sh:  error:  diff for sample s2:"
	cat samples/s2/diff.txt
fi

echo "==============================================================================="
echo ""
echo "Total number of tests  = $ntotal"
echo "Number of failed tests = $nfail"
echo "Done!"
echo ""

