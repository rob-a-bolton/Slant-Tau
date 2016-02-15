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
         "markov.rkt")

;; Test generation using test data
(test-begin
  (let ([my-hash (make-weak-hash)]
        [depth 4]
        [num-words 100])
    (with-input-from-file "data/test-data.txt"
      (lambda ()
        (train my-hash depth)))
    (check > (length (hash-keys my-hash)) 0)
    (check > (string-length (generate my-hash
                                      (- depth 1)
                                      num-words))
           0)))
