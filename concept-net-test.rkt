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
         json
         net/http-client
         (prefix-in cn:
                    "concept-net.rkt"))

(define (url . parts)
  (string-join parts "/"))

(define (json-get host url)
  (let-values (((status header port) (http-sendrecv host url)))
    (read-json port)))

(define concept-net-host "conceptnet5.media.mit.edu")
(define concept-net-url "http://conceptnet5.media.mit.edu/data/5.4")
(test-begin
  (let ((toast-page (json-get concept-net-host
                              (url concept-net-url
                                   "c" "en" "toast")))
        (toast-def (cn:get-concept "toast")))
    (check-equal? toast-def
                  toast-page)
    (check-equal?
     '(("/r/MadeOf" . "bread"))
      (map (λ (edge)
             (cons (hash-ref edge 'rel)
                   (hash-ref edge 'surfaceEnd)))
           (hash-ref (cn:search '((start . "/c/en/toast/")
                                  (rel . "/r/MadeOf/")))
                     'edges)))))
      

(test-begin
  (let ((t-assocs-raw (set "toast"
                           "toaster"
                           "breadcrumb"
                           "challah"
                           "laverbread"))
        (t-assocs-lib (list->set (cn:get-related (list "toast") 5))))
    (check-equal? t-assocs-lib
                  t-assocs-raw)))

(test-begin
  (let ((bottle-types (set "v" "n"))
        (bottle-types-lib (force (cn:get-word-types "bottle"))))
    (check-equal? bottle-types-lib
                  bottle-types)))

(test-begin
  (let ((expected-types (make-hash `(("v" . ,(mutable-set "bottle"))
                                     ("n" . ,(mutable-set "bottle")))))
        (split-types (force (cn:split-words-by-type (list "bottle")))))
    (check-equal? split-types
                  expected-types)))
