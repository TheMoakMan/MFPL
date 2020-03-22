#!/bin/bash

flex moake.l
bison moake.y
g++ moake.tab.c -o moake_parser

./moake_parser < $1 > myout.txt
