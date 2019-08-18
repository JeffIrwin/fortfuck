#!/bin/bash

./build.sh

echo "==============================================================================="
echo ""
echo "Running tests..."
echo ""

nfail=0
ntotal=0

NOPIE=
if [[ -e "/etc/debian_version" ]]; then
	NOPIE=-no-pie
fi

for b in ./samples/*/*.b; do

	d=$(dirname "$b")
	s="${b%.b}.s"

	#echo "b = $b"
	#echo "s = $s"
	#echo "d = $d"
	#echo ""

	ntotal=$((ntotal + 1))
	rm "$s"
	./fbc "$b"
	rm a.out
	gcc "$s" ${NOPIE}
	rm "$d"/output.txt
	./a.out < "$d"/input.txt > "$d"/output.txt
	diff "$d"/expected-output.txt "$d"/output.txt > "$d"/diff.txt
	if [[ "$?" == "1" ]]; then
		nfail=$((nfail + 1))
		echo "test.sh:  error:  diff for $b:"
		cat "$d"/diff.txt
	fi

done

echo ""
echo "==============================================================================="
echo ""
echo "Total number of tests  = $ntotal"
echo "Number of failed tests = $nfail"
echo "Done!"
echo ""

