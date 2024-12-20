#!/bin/bash

if [[ $2 -eq 1 ]];
then
    # run with test input
    cd test_inputs
else
    # run with actual input
    cd inputs
fi
python3 ../parser.py "$1"

cd ..
cd fortran

gfortran -c list.f90
gfortran -c string.f90
gfortran -c reader.f90
gfortran -c integer.f90
gfortran -c intcode.f90
gfortran -c types.f90
gfortran -c "$1.f90"
gfortran "$1".o list.o string.o reader.o integer.o intcode.o types.o -o "$1".out
./"$1".out