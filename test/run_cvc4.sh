#!/bin/bash

cvc4=./cvc4
bench="$1"

function trywith {
  ($cvc4 --lang=sygus --no-checking --no-interactive --dump-synth --default-dag-thresh=0 "$@" $bench) 2>/dev/null |
  (read result w1;
  case "$result" in
      unsat) echo "$w1";cat;exit 0;;
      unknown) echo "unknown";cat;exit 0;;
  esac; exit 1)
  if [ ${PIPESTATUS[1]} -eq 0 ]; then exit 0; fi
}

trywith --decision=internal --cbqi-prereg-inst
 
