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

(provide get-def)

(define def-net-host (make-parameter "conceptnet5.media.mit.edu"))
(define def-net-url  (make-parameter "http://conceptnet5.media.mit.edu/data/5.4"))
(define def-lang (make-parameter "en"))

(define (parts->url . parts)
  (string->url (string-join parts "/")))

(define (get-def concept
                 (host (def-net-host))
                 (url (def-net-url))
                 (lang (def-lang)))
  (call/input-url (parts->url url "c" lang concept)
                  get-pure-port
    (λ (port)
      (read-json port))))

                    
