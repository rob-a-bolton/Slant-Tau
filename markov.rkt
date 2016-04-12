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

(require racket/generator
         racket/hash
         db)

(provide train
         generate
         merge-word-hashes)

(define (consume-blank-lines)
  (let consume ((newlines 0)
                (line (read-line)))
    (cond
      ((eof-object? line)
       (values newlines #f))
      ((equal? line "")
       (consume (+ newlines 1) (read-line)))
      (else
       (values newlines line)))))

(define (1+ num)
  (+ 1 num))

(define (make-word-cols depth)
  (string-join
   (map (λ (num)
          (format "word_~a VARCHAR(32) REFERENCES words(word)"
                  num))
        (range depth))
   ", "))

(define (make-word-col-names depth)
  (string-join
   (map (λ (num)
          (format "word_~a" num))
        (range depth))
   ","))
                  

(define (make-vals-template num-cols num-vals)
  (string-join (make-list num-vals
                          (string-join (make-list num-cols "?")
                                       ","
                                       #:before-first "("
                                       #:after-last ")"))
               ","))
   

(define (generate-table db-con depth)
  (query-exec db-con (string-append "CREATE TABLE IF NOT EXISTS words("
                                    "word VARCHAR(32) PRIMARY KEY"
                                    ")"))
  (query-exec db-con  "DROP TABLE IF EXISTS markov")
  (query-exec db-con (format  "CREATE TABLE markov(~a, frequency INTEGER DEFAULT 0)" (make-word-cols depth))))

(define (word-generator)
"Generator to output one word at a time from the current
input port."
  (generator ()
    (yield 'start)
    (let-values (((newlines line) (consume-blank-lines)))
      (let gen-loop ((newlines newlines)
                     (line line))
        (cond
          ((= newlines 1)
           (yield 'break))
          ((> newlines 1)
           (begin (yield 'end) (yield 'start))))
        (if (not line)
            (yield #f)
            (begin
              (for-each yield (string-split line))
              (let-values (((newlines line) (consume-blank-lines)))
                (if (eof-object? line)
                    (yield 'end)
                    (begin
                      (when line (yield 'line-break))
                      (gen-loop newlines line))))))))))

(define (merge-words hash-1 hash-2)
"Merges two word hashes"
  (hash-union hash-1 hash-2 #:combine/key (λ (k v1 v2) (+ v1 v2))))

(define (get-unique-words word-hash)
"Returns all the unique words in the word hash."
  (let ((unique-words (make-hash)))
    (hash-for-each word-hash (λ (words freq)
                               (for-each (λ (word)
                                           (hash-set! unique-words word #t))
                                         words)))
    (hash-keys unique-words)))

(define (get-word-insert-vals word-hash)
  (flatten (hash->list word-hash)))

(define (upsert-words db-con word-hash depth)
"Inserts or updates the given words in a database."
  (let* (;(updated-hash (merge-words word-hash (pull-words db-con word-hash)))
         (unique-words (get-unique-words word-hash))
         (word-insert-query (format "INSERT IGNORE INTO words VALUES ~a"
                                   (make-vals-template 1 (length unique-words))))
         (temp-insert-query (format "INSERT INTO markov_merge VALUES ~a"
                                   (make-vals-template (1+ depth) (hash-count word-hash))))
         (markov-insert-query (format "INSERT OR IGNORE INTO markov (~a) VALUES ~a"
                                      (make-word-col-names depth)
                                      (make-vals-template depth (hash-count word-hash))))
         (markov-update-query (string-join (list
              "UPDATE markov"
              "NATURAL JOIN markov_merge"
              "SET markov.frequency = markov.frequency + markov_merge.new_frequency"))))
    (apply query-exec (append (list db-con word-insert-query) unique-words))
    (query-exec db-con "DROP TABLE IF EXISTS markov_merge")
    (query-exec db-con (format "CREATE TABLE markov_merge(~a, new_frequency INTEGER)" (make-word-cols depth)))
    (apply query-exec (append (list db-con temp-insert-query) (get-word-insert-vals word-hash)))
    (apply query-exec (append (list db-con markov-insert-query) (flatten (hash-keys word-hash))))
    (query-exec db-con markov-update-query)))
          

(define (make-word-query depth)
  (let* ((q (format "SELECT w_id_~a")))
    q))

(define (get-words db-con words (not-found #f))
  (let* ((rows (query-rows db-con (make-word-query (length words)))))
    (if (> 0 (length rows))
        rows
        not-found)))
    
        
   
(define (get-in hashmap keys (not-found #f))
"Gets a value from a set of nested hash maps from a list
of keys"
  (let get-loop ((sub-map hashmap)
                 (keys keys))
    (cond
      ((empty? keys)
       sub-map)
      ((hash? sub-map)
       (get-loop (hash-ref sub-map (car keys) not-found) (cdr keys)))
      (else
       not-found))))

(define (set-in *hashmap* keys val)
"Sets a value in a set of nested hash maps using a list
of keys"
  (let set-loop ((*sub-map* *hashmap*)
                 (keys keys))
    (let* ((key (first keys))
           (current-value (hash-ref *sub-map* key #f)))
      (cond
        ((empty? (cdr keys))
         (hash-set! *sub-map* key val))
        ((hash? current-value)
         (set-loop current-value (cdr keys)))
        (else
         (let ((new-hash (make-hash)))
           (hash-set! *sub-map* key new-hash)
           (set-loop new-hash (cdr keys))))))))

(define (update-word-hash *hash* words)
"Increments (or creates) the number associated with a
nested set of words in a word hash"
  (let* ((current-value (get-in *hash* words 0)))
    (set-in *hash* words (1+ current-value))))

(define (merge-word-hashes *hash-1* hash-2)
"Merges two word hashes"
  (letrec ((merge-vals (λ (k v1 v2)
                         (if (and (number? v1) (number? v2))
                             (+ v1 v2)
                             (begin
                               (hash-union! v1 v2 #:combine/key merge-vals)
                               v1)))))
    (hash-union! *hash-1* hash-2 #:combine/key merge-vals)))

(define (train *hash* depth)
"Modifies a word hash from the current input port."
  (let* ((g (word-generator)))
    (let train-loop ((words (list (g))))
      (cond
        ((not (first words))
         *hash*)
        ((and (> (length words) 1)
              (equal? 'start (first words))
              (equal? 'end (second words)))
         (train-loop '(start)))
        ((< (length words) depth)
         (train-loop (cons (g) words)))
        (else
         (begin
           (update-word-hash *hash* (reverse words))
           (train-loop (cons (g) (drop-right words 1)))))))))

(define (zip . lists)
"Zips two or more lists together."
  (let ziploop ((lst '())
                (lists lists))
    (if (ormap empty? lists)
        (reverse lst)
        (ziploop (cons (map first lists) lst)
                 (map cdr lists)))))

(define (get-weight key val)
"Gets the weight of a node in a tree of hashes. Leaf values
are expected to be a number."
  (if (number? val)
      val
      (foldl + 0 (hash-map val get-weight))))

(define (get-weighted-words word-hash words)
"Gets a weighted list of potential words from a word
hash and list of words."
  (let ((word-tree (get-in word-hash words)))
    (if (not word-tree)
        #f
        (zip (hash-keys word-tree)
             (hash-map word-tree get-weight)))))

(define (choose-weighted-word weighted-words)
"Picks a word at random from a weighted list of words."
  (let loop ((num (random (1+ (foldl + 0 (map cadr weighted-words)))))
             (words weighted-words))
    (let-values (((word weight) (apply values (car words))))
      (if (>= weight num)
          word
          (loop (- num weight) (cdr words))))))

(define (choose-word word-hash words lower-threshold upper-threshold)
"Picks a word based on the words already we already have."
  (let gen-loop ((words words))
    (let ((weighted-words (get-weighted-words word-hash words)))
      (cond
        ((and (not weighted-words) (= (length words) 1))
         #f)
        ((and (or (not weighted-words) (> (random lower-threshold (1+ upper-threshold))
                                          (length weighted-words)))
              (> (length words) 1))
         (gen-loop (drop words 1)))
        (else
         (choose-weighted-word weighted-words))))))

(define (text-fold word output-text)
  (match word
    ((or "." "?" "," "!" ":" "-")
     (string-append output-text word))
    ((or 'break 'end)
     (string-append output-text "\n\n"))
    ('line-break
     (string-append output-text "\n"))
    ('start
     output-text)
    (_
     (string-append output-text " " word))))

(define (generate word-hash depth num-words lower-threshold upper-threshold)
"Generates a number of words using a word hash and given
chain/state depth."
  (foldl text-fold ""
    (let gen-loop ((words '(start))
                   (current-length 1))
      (let* ((ordered-words (reverse (take words (min depth current-length))))
             (word (choose-word word-hash ordered-words lower-threshold upper-threshold)))
        (if (or (not word) (> current-length num-words))
            (drop (reverse words) 1)
            (gen-loop (cons word words) (1+ current-length)))))))
