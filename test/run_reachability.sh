#!/bin/sh

tests="thermostat cinderella3 cinderella2.5 cinderella2 cinderella1.8 cinderella1.7 cinderella1.6 cinderella1.5 cinderella1.4 nim44 nim45 nim55 nim56 nim66 nim222 nim123 nim233 nim333 nim444 nim555 nim556 nim2222 nim2223 tag_safe tag_reach "

mkdir -p output

echo "Name"
echo "-----------------------------------------"
for test in $tests; do
    printf "%-17s" $test

    start=$(date +"%s%3N")
    { timeout 600 ../simsat -synth reachability/$test.rg 2>&1; } > output/$test.simsynth.output
    status=$?
    end=$(date +"%s%3N")
    time=$(( $end - $start ))
    if [ $status -eq 124 ]; then
	time="TO"
    elif [ $status -ne 0 ]; then
	time="error"
    fi
    printf "%8s" $time

    echo ""
done
