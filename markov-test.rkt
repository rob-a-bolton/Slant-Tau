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

(require rackunit
         db
         "markov.rkt")

(require/expose "markov.rkt"
                (consume-blank-lines
                 word-generator))

;; Test generation using test data

(define db-con (mysql-connect #:database "slant_tau_test" #:user "markov" #:password "markov"))
(define (reset-tables)
  (query-exec db-con "DROP TABLES markov;")
  (query-exec db-con "DROP TABLES words;"))

(test-begin
  (reset-tables)
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

(test-begin
  (let-values (((blanks line)
                (with-input-from-string "\n\n\nthree blank lines"
                  (λ () (consume-blank-lines)))))
    (check-equal? blanks
                  3)
    (check-equal? line
                  "three blank lines")))

(test-begin
  (with-input-from-string "This is a line\nThis is another\n\nBreak\nEnd"
    (λ ()
      (let* ((wg (word-generator))
             (text (let consume-input ((words '()))
                     (let ((word (wg)))
                       (if word
                           (consume-input (cons word words))
                           (reverse words))))))
        (check-equal? text
                      (list "#_START" "This" "is" "a" "line" "#_LINE_BREAK"
                            "This" "is" "another" "#_BREAK"
                            "Break" "#_LINE_BREAK" "End" "#_END"))))))
         

    
