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

(require rackunit
         db
         "markov.rkt")

;; Test generation using test data

(define db-con (mysql-connect #:database "slant_tau_test" #:user "markov" #:password "markov"))

(test-begin
  (let ([depth 4]
        [num-words 100])
    (generate-tables db-con depth)
    (with-input-from-file "data/test-data.txt"
      (lambda ()
        (train db-con depth 1000)))
    (check > (string-length (generate db-con
                                      (- depth 1)
                                      num-words
                                      1
                                      3))
             0)))
