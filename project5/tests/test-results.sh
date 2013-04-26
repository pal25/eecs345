PASSED=$(grep -c 'pass' ./tests/results.txt)
TESTS=$(wc -l ./tests/results.txt | cut -f1 -d" ")
echo "Tests passed [$PASSED/$TESTS]"
rm -f ./tests/results.txt
