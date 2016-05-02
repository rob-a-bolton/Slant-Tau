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
         web-server/dispatch)

(define-values (slant-dispatch slant-urls)
  (dispatch-rules
   [("train") train-data]
   [("generate") generate-poem]
   [("def.css") serve-css]
   [("app.js") serve-app-js]
   [else serve-index]))

(define root-dir (path->string (current-directory)))

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
                 (form
                  (fieldset
                   (legend "Upload")
                   "Input text:" (br)
                   (input ((name "file") (type "file")))
                   (input ((type "submit"))))
                  (fieldset
                   (legend "Generate")
                   "Length:" (br)
                   (input ((name "length") (type "number")))
                   (input ((type "submit")))))))))))

(define (train-data req)
  (response/xexpr
   `(body "uh oh")))

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
               #:stateless? #t
               #:servlet-regexp #rx"")
