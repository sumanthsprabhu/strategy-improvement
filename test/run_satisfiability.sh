#!/bin/sh

tests="max15 array_search15 array_sum8_15 tenfuncs2 polynomial4 hms scaleweights lub10 inverse10 round10 puzzle35 puzzle35_opt"

#uncomment to run Alchemist / CVC4
# requires cvc4 and AlchemistDT / uss_static to be in this directory.
ALCHEMIST="  Alchemist"
CVC4="     CVC4"
run () {
    cmd=$1
    out=$2
    start=$(date +"%s%3N")
    { timeout 600 $cmd 2>&1; } > $out
    status=$?
    end=$(date +"%s%3N")
    time=$(( $end - $start ))
    if [ $status -eq 124 ]; then
	time="TO"
    elif [ $status -ne 0 ]; then
	time="error"
    fi
    printf "%9s" $time
}

mkdir -p output

echo "Name           $ALCHEMIST$CVC4 SimSynth"
                                    
echo "--------------------------------------------"
for test in $tests; do
    printf "%-17s" $test
    if [ "$ALCHEMIST" != "" ]; then
	run "./run_alchemist.sh satisfiability/$test.sl" "output/$test.alchemist.output"
    fi
    if [ "$CVC4" != "" ]; then
	run "./run_cvc4.sh satisfiability/$test.sl" "output/$test.cvc4.output"
    fi

    run "../simsat.native -synth satisfiability/$test.smt2" "output/$test.simsynth.output"

    echo ""
done
