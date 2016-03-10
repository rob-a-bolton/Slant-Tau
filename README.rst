Slant-Tau
=========

My dissertation project, a poetry generator. Early WIP.

Usage
=====
Although the focus of this repository is for the library,
an end-user tool is included for demonstrative purposes.
It is not advised to use this tool for large amounts of generation as it takes some time to launch and/or import/train on data each time it is launched.

Run the program as *racket main.rkt <args>*.
An executable can be produced by running *raco exe main.rkt*

The arguments are mostly commands and can be chained. The minimum requirement for generating any text is to run at least one train/import command and a generation command. The following commands are available:

  - -t, --train <file>: Train on an input corpus.
  - -i, --import <file>: Import some previously exported training data.
  - -e, --export <file>: Export currently loaded training data.
  - -g, --generate <num-words>: Generate *num-words* number of words of text.
  - --min-threshold <number>: Minimum number of matches to aim for when choosing a word
  - --max-threshold <number>: Stop widening word choices when this many (or more) chocies are available
  - -s, --seed <number>: Seed the random number generator
