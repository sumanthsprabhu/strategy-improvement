#!/bin/bash

./AlchemistDT  $1 2>/dev/null

if [ $? -ne 0 ]
then  
./uss-static lia $1  2>/dev/null
fi
