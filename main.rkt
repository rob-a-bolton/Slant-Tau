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

#lang racket/base

(module+ test
  (require rackunit))

(module+ main
  (require racket/cmdline
           racket/list
           racket/match
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
  (define operation-list
    (let* ((oplist (make-parameter '()))
           (add-op (λ (op-name file-name)
                     (oplist (cons (cons op-name file-name)
                                   (oplist))))))
      (command-line
       #:program program-name
       #:once-each
       [("-V" "--version") "Prints the program version."
                           (begin
                             (print-version)
                             (exit))]
       #:multi
       [("-t" "--train")    file-name
                            "Train on given file."
                            (add-op 'train file-name)]
       [("-g" "--generate") num-words
                            "Generate a number of words."
                            (add-op 'generate num-words)]
       [("-i" "--import")   file-name
                            "Import training data from file."
                            (add-op 'import file-name)]
       [("-e" "--export")   file-name
                            "Export training data to file."
                            (add-op 'export file-name)]
       [("--min-threshold") number
                            "Sets the lower bound for minimum number of words to accept in word choice."
                            (add-op 'min-threshold number)]
       [("--max-threshold") number
                            "Sets the upper bound for minimum number of words to accept in word choice."
                            (add-op 'max-threshold number)]
       [("-s" "--seed")     number
                            "Sets the random number generator's seed value."
                            (add-op 'seed number)]
         
       #:handlers (λ (args) (reverse (oplist)))
                  '()))) ; Inelegant, but required
  (let ([*word-hash* (make-hash)]
        [min-threshold (make-parameter 1)]
        [max-threshold (make-parameter 3)])
    (for ((op-pair operation-list))
      (let ((op (car op-pair))
            (arg (cdr op-pair)))
        (match op
          ['train
           (with-input-from-file arg
             (λ ()
               (train *word-hash* 10)))]
          ['generate
           (begin
             (display (generate *word-hash*
                              9
                              (string->number arg)
                              (min-threshold)
                              (max-threshold)))
             (newline))]
          ['import
           (merge-word-hashes *word-hash*
                              (call-with-input-file arg
                                (λ (port) (read port))))]
          ['export
           (call-with-output-file arg
             (λ (port)
               (write *word-hash* port)))]
          ['min-threshold
           (min-threshold (string->number arg))]
          ['max-threshold
           (max-threshold (string->number arg))]
          ['seed
           (random-seed (string->number arg))]))))
  )
