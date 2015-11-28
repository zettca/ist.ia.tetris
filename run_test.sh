#!/bin/bash
# Run test (for IA, clisp)
# 2015-11-09 by Luis
# Modified: 2015-11-28 (add count and colors)

# Arguments: <folder> <test name>
# example:

# run all tests in testes2
# ./run_test.sh testes2

# run testes2/test14
# ./run_test.sh testes2 test14




#######################
# Text formating strings
red_text="\x1b[31;1m"
green_text="\x1b[32;1m"
yellow_text="\x1b[33;1m"
gray_text="\x1b[90;1m"
blue_background="\x1b[46;1m"
gray_background="\x1b[100;1m"
black_background="\x1b[40;1m"
reset_text="\x1b[0m"
#######################

TOTAL=0
PASS=0
FAIL=0


function test(){

	TEST="$1"
	IN="$TEST/input"
	OUT="$TEST/output"
	OUT_FILTERED="$TEST/output_filtered"
	OUT_RESULT="$TEST/output_result"

	if [ -e $TEST ]; then

		TOTAL=$((TOTAL+1))

		echo ""
		echo -ne $gray_background
		echo -e "================ TEST #$TOTAL ================"
		echo $TEST

		# Filter commends out of expected output
		if [ -e  $OUT_FILTERED ]; then
			echo OK: Filtered exists.
		else
			sed -e '/^;/d' $OUT > $OUT_FILTERED
		fi

		echo -e $reset_text



		# Run the beast
		clisp -repl "tg031.lisp" < $IN 1> $OUT_RESULT

		colordiff $OUT_FILTERED $OUT_RESULT
		if [ $? -eq 0 ]; then
			echo -ne $green_text
			echo -ne "-- OK! --"
			echo -e $reset_text
			PASS=$((PASS+1))
		else
			echo -ne $red_text
			echo -ne "-- fuck --"
			echo -e $reset_text
			FAIL=$((FAIL+1))
			exit
		fi

	else
		echo "Test $1 does not exist."
	fi

}


if [ -n "$2" ]; then
	# run one test
	echo Running test "$1/$2"
	test "$1/$2"

elif [ -n "$1" ]; then
	echo "Running all tests in $1..."

	for t in $1/*
	do
		test $t
	done


	echo "========================================="

	echo -ne $red_text
	echo -e "Fail:\t$FAIL"
	echo -ne "$reset_text"

	echo -ne $green_text
	echo -e "Pass:\t$PASS"
	echo -ne "$reset_text"
	echo -e "Total:\t$TOTAL"
	echo "========================================="

else
	echo "args: <folder> <test>"
fi
