Slant-Tau
=========

My dissertation project, a poetry generator.

Usage
=====
Although the focus of this repository is for the library,
an end-user tool is included for demonstrative purposes.

Run the program as *racket main.rkt <args>*.
An executable can be produced by running *raco exe main.rkt*

The arguments are mostly commands and can be chained. The minimum requirement for generating any text is to run at least one train/import command and a generation command. The following commands are available:

  - -t, --train <file>: Train on an input corpus.
  - -g, --generate <num-words>: Generate *num-words* number of words of text.
  - -G, --generate-tables: Generates the MySQL tables
  - --drop-tables: Drops the MySQL tables
  - -d, --depth: Sets the depth for training/generation
  - -c, --cache-size: Sets the cache size for training (number of entries, not bytes)
  - -s, --seed: Sets the seed for generation
  - -w, --theme-words: Sets the theme words (must be comma delimited)
  - -r, --replace-chance: Sets the chance for word substitution (between 0 and 1)
  - --word-pool: Sets the scale factor for number of potential replacement words compared to words generated.
  - -u, --username: MySQL db username
  - -p, --password: MySQL db password
  - -n, --database-name: MySQL db name
  - --min-threshold <number>: Minimum number of matches to aim for when choosing a word
  - --max-threshold <number>: Stop widening word choices when this many (or more) chocies are available
  - -s, --seed <number>: Seed the random number generator
