#!/bin/bash
# Run test
# 2015-11-09 by Luis

# Arguments: test number, such as 02

TEST="testes/test$1"
echo $TEST
IN="$TEST/input"
OUT="$TEST/output"
OUT_FILTERED="$TEST/output_filtered"
OUT_RESULT="$TEST/output_result"

if [ -e $TEST ]; then

	# Filter commends out of expected output
	if [ -e  $OUT_FILTERED ]; then
		echo OK: Filtered exists.
	else
		sed -e '/^;/d' $OUT > $OUT_FILTERED
	fi


	# Run the beast
	clisp -repl "tg31.lisp" < $IN > $OUT_RESULT

	colordiff $OUT_FILTERED $OUT_RESULT

else
	echo "Test $1 does not exist."
fi
