#!/bin/sh

if [ -z "$1" ]
then
    echo "You need to specify the name of the data file"
    exit 1
fi

gnuplot -e "set terminal png; set output '$1.png'; set style data linespoints; plot '$1.dat'"
