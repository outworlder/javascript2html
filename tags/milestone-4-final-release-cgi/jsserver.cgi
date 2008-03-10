#!/bin/sh
export CHICKEN_REPOSITORY=/home/protected/chicken_eggs
export LD_LIBRARY_PATH=/home/protected/chicken_bin/lib:$LD_LIBRARY_PATH
./jsserver
