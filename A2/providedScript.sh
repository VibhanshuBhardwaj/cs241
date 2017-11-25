#!/bin/bash

echo java cs241.wordasm '<' $1.hex '>' $1.mips
java cs241.wordasm < $1.hex > $1.mips
if [ $? -eq 0 ]
  then
    echo java mips.twoints $1.mips
    java mips.twoints $1.mips
  fi