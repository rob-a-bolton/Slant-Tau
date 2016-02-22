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
         racket/hash)

(provide train
         generate
         merge-word-hashes)

(define (word-generator)
"Generator to output one word at a time from the current
input port."
  (generator ()
    (yield 'start)
    (let loop ((line (read-line)))
      (if (eof-object? line)
          (yield 'end)
          (begin
            (for-each yield (string-split line))
            (loop (read-line)))))))

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
    (set-in *hash* words (+ current-value 1))))

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
        ((equal? 'end (first words))
         *hash*)
        ((< (length words) depth)
              (train-loop (cons (g) words)))
        ((begin
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
  (let loop ((num (random (foldl + 0 (map cadr weighted-words))))
             (words weighted-words))
    (let-values (((word weight) (apply values (car words))))
      (if (>= weight num)
          word
          (loop (- num weight) (cdr words))))))  

(define (choose-word word-hash words)
"Picks a word based on the words already we already have."
  (let ((weighted-words (get-weighted-words word-hash words)))
    (if (not weighted-words)
        #f
        (choose-weighted-word weighted-words))))

(define (generate word-hash depth num-words)
"Generates a number of words using a word hash and given
chain/state depth."
  (string-join
    (let gen-loop ((words '(start))
                   (current-length 1))
      (let* ((ordered-words (reverse (take words (min depth current-length))))
             (word (choose-word word-hash ordered-words)))
        (if (or (not word) (> current-length num-words))
            (drop (reverse words) 1)
            (gen-loop (cons word words) (+ current-length 1)))))
    " "))

