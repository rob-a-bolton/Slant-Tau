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
         json)

(provide get-concept
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
                      
    
