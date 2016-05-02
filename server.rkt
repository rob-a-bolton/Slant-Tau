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

(define (get-file name)
  (string-append root-dir "/" name))

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
                           (type "submit"))))
                  (form ((onsubmit "generate(); return false;"))
                   (legend "Generate")
                   "Length:" (br)
                   (input ((id "filename")
                           (name "length")
                           (type "number")
                           (min "1")
                           (max "500")))
                   (input ((id "generate-button")
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
  (response/xexpr
   `(body "oops")))

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
