#!/bin/sh
set -e
ifort -o read_from_matlab read_from_matlab.f90
./read_from_matlab 
exit
