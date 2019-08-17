#!/bin/bash

./build.sh

echo "Running tests..."

./fbc samples/s0/add.b
gcc samples/s0/add.s -no-pie
./a.out < samples/s0/input.txt > samples/s0/output.txt
diff samples/s0/expected-output.txt samples/s0/output.txt > samples/s0/diff.txt
if [[ "$?" == "1" ]]; then
	echo "test.sh:  error:  diff for sample s0:"
	cat samples/s0/diff.txt
fi

echo "Done!"
echo ""

