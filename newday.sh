#!/bin/sh

check=$(echo $1 | sed -E 's/^[012][0-9][ab]$/VALID/')
if [ x$check != 'xVALID' ]; then
    echo "USAGE: $0 <[012][0-9][ab]>"
    exit
fi

day=$1
echo "Creating files for day$day:"
make src/day${day}.f90
make src/day${day}_main.f90
make src/day${day}_test.f90
make fruitpy/day${day}.py
touch inputfiles/day${day%?}_example.txt
touch inputfiles/day${day%?}_input.txt
ls -l inputfiles/day${day%?}_*.txt
