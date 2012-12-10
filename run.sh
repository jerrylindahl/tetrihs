#!/bin/bash
rm *.hi
rm *.o
ghc --make Tetrihs.hs -o Tetris && ./Tetris
