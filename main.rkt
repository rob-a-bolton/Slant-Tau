;; slant-tau
;; Copyright (c) 2016 Robert Alexander Bolton
;; 
;; This package is distributed under the GNU Lesser General Public
;; License (LGPL).  This means that you can link slant-tau into proprietary
;; applications, provided you follow the rules stated in the LGPL.  You
;; can also modify this package; if you distribute a modified version,
;; you must distribute it under the terms of the LGPL, which in
;; particular means that you must release the source code for the
;; modified software.  See http://www.gnu.org/copyleft/lesser.html
;; for more information.

#lang racket

(module+ test
  (require rackunit))

(module+ main
  (require racket/cmdline
           racket/list
           racket/match
           db
           "markov.rkt"))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco doc <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(define program-version '(0 0 1))
(define program-name "slant-tau")
(define (print-version)
  (apply fprintf `(,(current-error-port)
                   "~a version ~a.~a.~a~n"
                   ,program-name
                   ,@program-version))) ; expand version

(define (exit-with-error msg)
  (print msg (current-error-port))
  (exit 1))

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  (let ([word-pool (make-parameter 3)]
        [command (make-parameter #f)]
        [num-words (make-parameter #f)]
        [file-name (make-parameter #f)]
        [choice-lower (make-parameter 2)]
        [choice-upper (make-parameter 4)]
        [depth (make-parameter 8)]
        [cache-size (make-parameter 1000)]
        [theme-words (make-parameter #f)]
        [replace-chance (make-parameter 0.5)]
        [db-username (make-parameter "slant-tau")]
        [db-name (make-parameter "slant_tau")]
        [db-password (make-parameter "slant-tau")])
      (command-line
       #:program program-name
       #:once-any
       [("-t" "--train")
          arg-file
          "Train on given file."
          (begin (command 'train)
                 (file-name arg-file))]
       [("-g" "--generate")
          number
          "Generate a number of words."
          (begin (command 'generate)
                 (num-words (string->number number)))]
       [("-G" "--generate-tables")
          "Sets up the MySQL database for training, with given depht."
          (command 'generate-tables)]
       [("--drop-tables")
          "Drops the database tables, clearing trained data."
          (command 'drop-table)]
       #:once-each
       [("-V" "--version") "Prints the program version."
                           (begin
                             (print-version)
                             (exit))]
       [("-d" "--depth")
          number
          "Sets the depth to use for training/generation."
          (depth (string->number number))]
       [("-c" "--cache-size")
          number
          "Sets the cache size for training."
          (cache-size (string->number number))]
       [("--min-threshold")
          number
          "Sets the lower bound for minimum number of words to accept in word choice."
          (choice-lower (string->number number))]
       [("--max-threshold")
          number
          "Sets the upper bound for minimum number of words to accept in word choice."
          (choice-upper (string->number number))]
       [("-s" "--seed")
          number
          "Sets the random number generator's seed value."
          (random-seed (string->number number))]
       [("-w" "--theme-words")
          words
          "Sets the theme words for generation."
          (theme-words (string-split words ","))]
       [("-r" "--replace-chance")
          number
          "Sets the chance of replacing a word (0.0 -> 1.0)."
          (replace-chance (string->number number))]
       [("--word-pool")
          number
          "Sets the scale factor for number of replacement words. Multiplies against the number of words generated."
          (word-pool (string->number number))]
       [("-u" "--username")
          username
          "The username for mysql."
          (db-username username)]
       [("-p" "--password")
          password
          "The password for mysql."
          (db-password password)]
       [("-n" "--database-name")
          name
          "The mysql database name."
          (db-name name)]
       #:ps
       " This work includes data from ConceptNet 5, which was compiled by the Commonsense Computing Initiative. ConceptNet 5 is freely available under the Creative Commons Attribution-ShareAlike license (CC BY SA 3.0) from http://conceptnet5.media.mit.edu. The included data was created by contributors to Commonsense Computing projects, contributors to Wikimedia projects, Games with a Purpose, Princeton University's WordNet, DBPedia, OpenCyc, and Umbel."
       )
      (if (not (command))
          (error "No command given.")
          (let ([db-con (mysql-connect #:database (db-name)
                                       #:user (db-username)
                                       #:password (db-password))])
            (cond
              ((equal? (command) 'train)
               (with-input-from-file (file-name)
                 (λ ()
                   (train db-con (depth) (cache-size)))))
              ((equal? (command) 'generate)
               (displayln (generate db-con (depth) (num-words)
                                    (choice-lower) (choice-upper)
                                    (theme-words)
                                    (replace-chance))))
              ((equal? (command) 'generate-tables)
               (generate-table db-con (depth)))
              ((equal? (command) 'drop-tables)
               (drop-tables))))))
  )
