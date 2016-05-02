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

(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         json
         "markov.rkt"
         db)

(define-values (slant-dispatch slant-urls)
  (dispatch-rules
   [("train") #:method "post" train-data]
   [("generate") generate-poem]
   [("def.css") serve-css]
   [("app.js") serve-app-js]
   [else serve-index]))

(define root-dir (path->string (current-directory)))
(define db-con (mysql-connect #:database "slant_tau_web"
                              #:user "slant-tau"
                              #:password "slant-tau"))
(define depth 8)

(define (get-file name)
  (string-append root-dir "/" name))

(define (limit lower upper val)
  (let ((real-val (if (string? val)
                      (string->number val)
                      val)))
  (if real-val
      (min (max lower real-val) upper)
      lower)))

(define (aget key lst)
  (let ((val (assoc key lst)))
    (if val
        (cdr val)
        #f)))

(define (parse-theme-words val)
  (let ((words (string-split (or val ""))))
    (if (not (empty? words))
        words
        #f)))

(define (serve-index req)
  (response/xexpr
   #:preamble #"<!doctype html>"
   `(html
     (head (title "Slant-Tau")
           (link ((rel "stylesheet") (href "def.css")))
           (script ((src "app.js"))))
     (body
      (main (div ((id "generate-box"))
                  (form ((onsubmit "uploadFile(); return false;"))
                   (legend "Upload")
                   "Input text:" (br)
                   (input ((id "file")
                           (name "file-name")
                           (type "file")))
                   (input ((id "file-upload-button")
                           (value "Upload")
                           (type "submit"))))
                  (form ((id "generate-form")
                         (onsubmit "generate(); return false;"))
                   (legend "Generate")
                   (label "Length (words, minimum)") (br)
                   (input ((name "length")
                           (type "number")
                           (min "1")
                           (max "500")
                           (value "100")))
                   (br)
                   (label "Theme words (space delimited)")
                   (input ((name "theme-words")
                           (type "text")))
                   (br)
                   (label "Replacement chance per word")
                   (input ((name "replace-chance")
                           (type "number")
                           (step "0.1")
                           (min "0.0")
                           (max "1.0")
                           (value "0.8")))
                   (br)
                   (label "Seed value") (br)
                   (input ((seed "seed")
                           (type "number")))
                   (br)
                   (label "Word pool size (as factor of generated words")
                   (input ((name "word-pool")
                           (type "number")
                           (min "1")
                           (max "5")
                           (value "3")))
                   (br)
                   (label "Fewest matches to aim for (per word)")
                   (input ((name "lower-threshold")
                           (type "number")
                           (min "1")
                           (max ,(number->string (- depth 2)))
                           (value "1")))
                   (br)
                   (label "Highest matches to aim for (per word)")
                   (input ((name "upper-threshold")
                           (type "number")
                           (min "2")
                           (max ,(number->string (- depth 1)))
                           (value "4")))
                   (br)
                   (input ((id "generate-button")
                           (value "Generate")
                           (type "submit"))))))))))

(define (train-data req)
  (if (not (request-post-data/raw req))
           (response 405 #"No POST data"
                     (current-seconds)
                     #f '() void)
           (let ((data (bytes->string/utf-8 (request-post-data/raw req))))
             (response 200 #"OK"
                       (current-seconds)
                       #"application/json"
                       '()
                       (curry try-train data)))))

(define (try-train data port)
  (write-json
   (hash 'successful
         (with-handlers
           ([exn:fail? (λ (exn) #f)])
           (with-input-from-string data
             (λ ()
               (train db-con 8 1000)
               #t))))
   port))

(define (generate-poem req)
  (let* ([params (request-bindings req)]
         [num-words (limit 1 500 (or (aget 'num-words params)
                                     100))]
         [lower-threshold (limit 1
                                 (- depth 1)
                                 (aget 'lower-threshold
                                        params))]
         [upper-threshold (limit lower-threshold
                                 (- depth 1)
                                 (aget 'upper-threshold
                                        params))]
         [theme-words-param (aget 'theme-words params)]
         [theme-words (parse-theme-words theme-words-param)]
         [replace-chance (limit 0.0
                                1.0
                                (aget 'replace-chance
                                       params))]
         [word-pool (limit 1 5 (aget 'word-pool params))]
         [text (with-handlers
                 ([exn:fail? (λ (exn) (displayln exn))])
                 (generate db-con
                           depth
                           num-words
                           lower-threshold
                           upper-threshold
                           theme-words
                           replace-chance
                           word-pool))])
    (displayln text)
    (response 200 #"OK"
              (current-seconds)
              #"application/json"
              '()
              (curry write-json
                     (hash 'successful (if text #t #f)
                           'text text)))))

(define (serve-css req)
  (serve-file "def.css"))

(define (serve-app-js req)
  (serve-file "app.js"))

(define (serve-file file)
  (let ((full-file-path (get-file file)))
    (if (file-exists? full-file-path)
        (response 200 #"OK"
                  (current-seconds)
                  #"text/css"
                  '()
                  (λ (out-port)
                    (write-bytes (file->bytes full-file-path)
                                 out-port)))
        (response 404 #"FILE NOT FOUND"
                  (current-seconds)
                  #f
                  '()
                  void))))

(serve/servlet slant-dispatch
               #:launch-browser? #f
               #:servlet-regexp #rx"")
