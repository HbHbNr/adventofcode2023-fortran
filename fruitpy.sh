#!/bin/bash

FRUITPYTESTS=$(make -pn | grep 'FRUITPYTESTS :=' | cut -d' ' -f 3-)

if [ x$1 = xQC ]; then
    echo 'Note: skipping long running test for day 05b'
    FRUITPYTESTS=$(echo $FRUITPYTESTS | sed 's# fruitpy/day05b.py##')
fi

for FRUITPYTEST in $FRUITPYTESTS; do
    echo "******************** ${FRUITPYTEST} ********************"
    python3 ${FRUITPYTEST}
done | grep -vE "^make: '[^']+' is up to date.$"
