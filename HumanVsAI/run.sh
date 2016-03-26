#!/bin/bash

cd pgm

gfortran \
    -g -fcheck=all -Wall -fbacktrace -O3 \
    setParameters.f90 \
    userSet.f90 \
    setVariables.f90 \
    main.f90 \
    move.f90 \
    output.f90 \
    evaluate.f90 \
    ai.f90 \
    -o ../run.exe

rm *.mod
