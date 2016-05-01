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

(require net/http-client
         net/url
         racket/set
         json)

(provide def-net-host
         def-net-port
         def-data-ver
         def-lang
         def-limit
         get-concept
         get-related
         get-word-types
         search)

(define def-net-host (make-parameter "conceptnet5.media.mit.edu"))
(define def-net-port (make-parameter #f))
(define def-data-ver  (make-parameter "5.4"))
(define def-lang (make-parameter "en"))
(define def-limit (make-parameter 50))

(define (parts->url . parts)
  (string->url (string-join parts "/")))

(define (get-concept concept
                     (lang (def-lang))
                     (host (def-net-host))
                     (port (def-net-port))
                     (ver (def-data-ver)))
  (let ((concept-url (make-url
                      "http"
                      #f
                      host
                      port
                      #t
                      (map (curryr path/param '())
                           (list "data" ver "c" lang concept))
                      '()
                      #f)))
    (call/input-url concept-url get-pure-port read-json)))

(define (search terms
                (lang (def-lang))
                (host (def-net-host))
                (port (def-net-port))
                (ver (def-data-ver)))
  (let ((search-url (make-url "http"
                              #f
                              host
                              port
                              #t
                              (map (curryr path/param '())
                                   (list "data" ver "search"))
                              terms
                              #f)))
    (call/input-url search-url get-pure-port read-json)))

(define (get-related words
                     num-results
                     (lang (def-lang))
                     (host (def-net-host))
                     (port (def-net-port))
                     (ver (def-data-ver)))
  (let* ((path-elems (list "data" ver "assoc" "list" lang
                           (string-join words ",")))
         (params `((filter . ,(string-append "/c/" lang))
                   (limit . ,(number->string num-results))))
         (query-url (make-url "http"
                              #f
                              host
                              port
                              #t
                              (map (curryr path/param '())
                                   path-elems)
                              params
                              #f)))
    (call/input-url query-url get-pure-port
      (λ (port)
        (let ((json-data (read-json port)))
          (map (λ (pair)
                 (cons (last (string-split (car pair) "/"))
                       (cadr pair)))
               (hash-ref json-data 'similar)))))))

(define (filter-irrelevant edge-hash word)
  (let ((new-edges
         (filter (λ (edge)
                   (or (equal? (hash-ref edge 'surfaceStart) word)
                       (equal? (hash-ref edge 'surfaceEnd) word)))
                 (hash-ref edge-hash 'edges))))
    (hash-set* edge-hash 'edges new-edges
                         'numFound (length new-edges))))

(define (get-word-types word
                        (types '("n" "v" "a"))
                        (lang (def-lang))
                        (host (def-net-host))
                        (port (def-net-port))
                        (ver (def-data-ver)))
  (let ((query-type
         (λ (type) (call/input-url
                     (make-url
                        "http"
                        #f
                        host
                        port
                        #t
                        (map (curryr path/param '())
                             (list "data" ver "c" lang word type))
                        '()
                        #f)
                     get-pure-port
                     (λ (port)
                       (let ((result (filter-irrelevant (read-json port) word)))
                         (if (> (hash-ref result 'numFound) 0)
                             type
                             #f)))))))
  (list->set (filter (compose not false?) (map query-type types)))))
