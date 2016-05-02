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
    (test-true "Connection possible"
      (jsexpr? toast-page))
    (test-true "get-concept gets definition"
      (equal? toast-def
              toast-page))
    (test-equal? "search works"
      '(("/r/MadeOf" . "bread"))
      (map (λ (edge)
             (cons (hash-ref edge 'rel)
                   (hash-ref edge 'surfaceEnd)))
           (hash-ref (cn:search '((start . "/c/en/toast/")
                                  (rel . "/r/MadeOf/")))
                     'edges)))))
      

(test-begin
  (let ((t-assocs-raw '(("toast" . 0.9991357567601089)
                        ("toaster" . 0.913973388310328)
                        ("breadcrumb" . 0.9001408372514137)
                        ("challah" . 0.8989284602769767)
                        ("laverbread" . 0.895879098474489)))
        (t-assocs-lib (cn:get-related (list "toast") 5)))
    (test-equal? "get-related works"
                 t-assocs-lib
                 t-assocs-raw)))

(test-begin
  (let ((toast-types (set "v" "n"))
        (toast-types-lib (cn:get-word-types "toast")))
    (test-equal? "get-types gets right types for toast"
                 toast-types-lib
                 toast-types)))

(test-begin
  (let ((expected-types (make-hash `(("a" . ,(mutable-set))
                                     ("v" . ,(mutable-set "toast"))
                                     ("n" . ,(mutable-set "toast")))))
        (split-types (cn:split-words-by-type (list "toast"))))
    (test-equal? "split-word-by-type correctly splits toast"
                 split-types
                 expected-types)))