#!/bin/bash
# Tests for the implementation, very basic but can be used to check if it *seems* to work.
# Supposed to be run from the root folder as `./tests/tests-launch`

tmpfile="./tests/tmp"
inputCurve="./tests/exampleInput"
inputCurve2="./tests/exampleInput2"
pyScript="./tests/outputTester.py"
pass=0
tests=1

./flp22-fun < $inputCurve > $tmpfile
diffres=$(cat $tmpfile)
rm $tmpfile
if [ "$diffres" == "flp22-fun - ECDSA
*****************
  - This program requires 1 user parameter and reads input from stdin.
  - Additional second argument may be used to refer to file from which the content will be read instead of stdin
  - Valid parameters (of which only 1 must be used at the time) are [-i|-k|-s|-v|-h] where:
      -i loads the input into the data structure and prints it out
      -k loads the input and prints out the generated key pair
      -s loads the input with a private key and a hash and prints out the generated signature
      -v loads the input with a public key and a hash and checks if it matches the signature
      -h prints this menu

Too many or too few arguments!" ]; then
    pass=$((pass+1))
    echo "Too few parameters - correct error - TEST $tests"
else
    echo "Too few parameters - passes when it shouldnt"
fi

tests=$((tests+1))

./flp22-fun -k -i -v < $inputCurve > $tmpfile
diffres=$(cat $tmpfile)
rm $tmpfile
if [ "$diffres" == "flp22-fun - ECDSA
*****************
  - This program requires 1 user parameter and reads input from stdin.
  - Additional second argument may be used to refer to file from which the content will be read instead of stdin
  - Valid parameters (of which only 1 must be used at the time) are [-i|-k|-s|-v|-h] where:
      -i loads the input into the data structure and prints it out
      -k loads the input and prints out the generated key pair
      -s loads the input with a private key and a hash and prints out the generated signature
      -v loads the input with a public key and a hash and checks if it matches the signature
      -h prints this menu

Too many or too few arguments!" ]; then
    pass=$((pass+1))
    echo "Too many parameters - correct error - TEST $tests"
else
    echo "Too many parameters - passes when it shouldnt"
fi

tests=$((tests+1))

./flp22-fun -h < $inputCurve > $tmpfile
diffres=$(cat $tmpfile)
rm $tmpfile
if [ "$diffres" == "flp22-fun - ECDSA
*****************
  - This program requires 1 user parameter and reads input from stdin.
  - Additional second argument may be used to refer to file from which the content will be read instead of stdin
  - Valid parameters (of which only 1 must be used at the time) are [-i|-k|-s|-v|-h] where:
      -i loads the input into the data structure and prints it out
      -k loads the input and prints out the generated key pair
      -s loads the input with a private key and a hash and prints out the generated signature
      -v loads the input with a public key and a hash and checks if it matches the signature
      -h prints this menu" ]; then
    pass=$((pass+1))
    echo "Parameter -h works correctly - TEST $tests"
else
    echo "Parameter -h doesn't work"
fi

tests=$((tests+1))

./flp22-fun -a < $inputCurve > $tmpfile
diffres=$(cat $tmpfile)
rm $tmpfile
if [ "$diffres" == "flp22-fun - ECDSA
*****************
  - This program requires 1 user parameter and reads input from stdin.
  - Additional second argument may be used to refer to file from which the content will be read instead of stdin
  - Valid parameters (of which only 1 must be used at the time) are [-i|-k|-s|-v|-h] where:
      -i loads the input into the data structure and prints it out
      -k loads the input and prints out the generated key pair
      -s loads the input with a private key and a hash and prints out the generated signature
      -v loads the input with a public key and a hash and checks if it matches the signature
      -h prints this menu

Unknown parameter!" ]; then
    pass=$((pass+1))
    echo "Unknown parameter works correctly - TEST $tests"
else
    echo "Unknown parameter doesn't work"
fi

tests=$((tests+1))

./flp22-fun -i < $inputCurve > $tmpfile
diffres=$(diff $tmpfile $inputCurve)
rm $tmpfile
if [ "$diffres" == "" ]; then
    pass=$((pass+1))
    echo "Parameter -i works correctly - TEST $tests"
else
    echo "$diffres"
fi

tests=$((tests+1))

./flp22-fun -i $inputCurve > $tmpfile
diffres=$(diff $tmpfile $inputCurve)
rm $tmpfile
if [ "$diffres" == "" ]; then
    pass=$((pass+1))
    echo "Reading from file instead of stdin works - TEST $tests"
else
    echo "Reading from file instead of stdin ERROR"
fi

tests=$((tests+1))

./flp22-fun -i < $inputCurve2 > $tmpfile
diffres=$(diff $tmpfile $inputCurve2)
rm $tmpfile
if [ "$diffres" == "" ]; then
    pass=$((pass+1))
    echo "Parameter -i works correctly - TEST $tests"
else
    echo "$diffres"
fi


# lets test the key 5 times because it's RNG and all outputs w/ valid input should be ok
for i in {1..5}
do
    tests=$((tests+1))
    ./flp22-fun -k < $inputCurve > $tmpfile
    python3 $pyScript -k < $tmpfile
    if [ $? == 42 ]; then
        pass=$((pass+1))
        echo "Parameter -k seems to produce a valid-ish looking output - TEST $tests"
    else
        echo "FAIL Parameter -k seems to have produced something weird"
    fi
    rm $tmpfile
done

# do the same with the -s parameter
for i in {1..5}
do
    tests=$((tests+1))
    ./flp22-fun -s < ./tests/exampleS > $tmpfile
    python3 $pyScript -s < $tmpfile
    if [ $? == 43 ]; then
        pass=$((pass+1))
        echo "Parameter -s seems to produce a valid-ish looking signature - TEST $tests"
    else
        echo "FAIL Parameter -s seems to have produced something weird"
    fi
    rm $tmpfile
done

# test my own input w/ custom generated key
for i in {1..5}
do
    tests=$((tests+1))
    ./flp22-fun -s < ./tests/exampleS1 > $tmpfile
    python3 $pyScript -s < $tmpfile
    if [ $? == 43 ]; then
        pass=$((pass+1))
        echo "Parameter -s seems to produce a valid-ish looking signature with my custom key - TEST $tests"
    else
        echo "FAIL Parameter -s seems to have produced something weird"
    fi
    rm $tmpfile
done

tests=$((tests+1))

# similarly, test the -v
./flp22-fun -v < ./tests/exampleV > $tmpfile
python3 $pyScript -vT < $tmpfile
if [ $? == 44 ]; then
    pass=$((pass+1))
    echo "Parameter -v seems to pass with expected input - TEST $tests"
else
    echo "FAIL Parameter -v seems to have produced something weird"
fi
rm $tmpfile

tests=$((tests+1))

# -v with custom generated input
./flp22-fun -v < ./tests/exampleV1 > $tmpfile
python3 $pyScript -vT < $tmpfile
if [ $? == 44 ]; then
    pass=$((pass+1))
    echo "Parameter -v seems to pass with my custom input - TEST $tests"
else
    echo "FAIL Parameter -v seems to have produced something weird"
fi
rm $tmpfile

tests=$((tests+1))

# -v with custom generated input but its supposed to fail
./flp22-fun -v < ./tests/exampleV1False > $tmpfile
python3 $pyScript -vF < $tmpfile
if [ $? == 45 ]; then
    pass=$((pass+1))
    echo "Parameter -v seems to correctly fail with my custom wrong key - TEST $tests"
else
    echo "FAIL Parameter -v seems to have produced something weird"
fi
rm $tmpfile

fail=25
echo "Testing is over, $pass tests passed, $((fail-pass)) failed"
