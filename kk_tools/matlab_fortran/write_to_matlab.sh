#!/bin/sh
set -e
ifort -o write_to_matlab write_to_matlab.f90
./write_to_matlab 
exit
