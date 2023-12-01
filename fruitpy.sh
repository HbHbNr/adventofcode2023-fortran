#!/bin/bash

FRUITPYTESTS=$(make -pn | grep 'FRUITPYTESTS :=' | cut -d' ' -f 3-)

for FRUITPYTEST in $FRUITPYTESTS; do
    echo "******************** ${FRUITPYTEST} ********************"
    python3 ${FRUITPYTEST}
done | grep -vE "^make: '[^']+' is up to date.$"
