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
         split-words-by-type
         replace-words
         search)

(define unreplaceable-words
  (set "." "?" "," "!" ":" "-"
       "#_BREAK" "#_END" "#_START" "#_LINE_BREAK"))

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
"Returns some edges for a concept."
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
"Searches ConceptNet using the given terms. Consult the 
ConceptNet wiki for information on what terms the search URI
can handle."
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
"Returns some words related to the supplied words."
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
          (set->list
           (list->set
            (append words
                    (map (λ (pair)
                           (last (string-split (car pair) "/")))
                         (hash-ref json-data 'similar))))))))))

(define (filter-irrelevant edge-hash word)
"Filters out any edges which are not directly related to the word."
  (let ((new-edges
         (filter (λ (edge)
                   (equal? (hash-ref edge 'end) word))
                 (hash-ref edge-hash 'edges))))
    (hash-set* edge-hash 'edges new-edges
                         'numFound (length new-edges))))

(define (get-word-types word
                        (word-types (set "v" "n" "a"))
                        (lang (def-lang))
                        (host (def-net-host))
                        (port (def-net-port))
                        (ver (def-data-ver)))
"Returns the types of ways a word can be used."
(delay/thread
 (list->set
  (filter (compose not false?)
    (map force
         (set-map
          word-types
          (λ (type)
            (delay/thread
             (call/input-url
              (make-url "http"
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
                (let ((num-found (hash-ref (read-json port)
                                           'numFound)))
                  (if (> num-found 0)
                      type
                      #f))))))))))))

(define (split-words-by-type words
                             (lang (def-lang))
                             (host (def-net-host))
                             (port (def-net-port))
                             (ver (def-data-ver)))
"Returns a hash of word types (e.g. 'n', 'v', 'a'), each
containing all of the given words which belong in one of
these categories."
(let ((type-hash (make-hash))
      (word-promises (map
                      (λ (word)
                        (delay/thread
                         (cons word
                               (force (get-word-types word)))))
                      words)))
  (for-each
   (λ (word-promise)
     (let* ((word-type-pair (force word-promise))
            (word (car word-type-pair))
            (types (cdr word-type-pair)))
       
       (set-for-each types
         (λ (type)
           (when (not (hash-has-key? type-hash type))
             (hash-set! type-hash type (mutable-set)))
           (set-add! (hash-ref type-hash type)
                     word)))))
   word-promises)
  type-hash))

(define (replace-word replacements word)
"Replaces a single word from a set of replacements."
  (let* ((word-types (set->list (force (get-word-types word))))
         (potential-words
          (set->list
           (apply set-union
                  (cons (set)
                        (map (λ (word-type)
                               (hash-ref replacements word-type set))
                             word-types))))))
    (if (not (empty? potential-words))
        (string-replace (list-ref potential-words
                                  (random (length potential-words)))
          "_" " ")
        word)))

(define (replace-words words
                       theme-words
                       (replacement-chance 0.5)
                       (word-pool 3))
"Replaces as many words as possible by substituting them from
 a set of new words generated from a list of theme words. word-pool
 is a multiplier against the number of words to produce a pool of
 replacements."
  (let* ((replaceable-words
          (filter (compose not
                           (curry set-member? unreplaceable-words))
                  words))
         (replacement-words
          (split-words-by-type
           (get-related theme-words
                        (* word-pool (length replaceable-words))))))
    (map (λ (word)
           (if (and (set-member? replaceable-words word)
                    (< (random) replacement-chance))
               (replace-word replacement-words word)
               word))
         words)))
