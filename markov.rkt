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
         "concept-net.rkt"
         db)

(provide train
         generate)

(define (consume-blank-lines)
  (let consume ((newlines 0)
                (line (read-line (current-input-port) 'any)))
    (cond
      ((eof-object? line)
       (values newlines #f))
      ((equal? line "")
       (consume (1+ newlines) (read-line (current-input-port) 'any)))
      (else
       (values newlines line)))))

(define (1+ num)
  (+ 1 num))

(define (make-word-query depth #:joiner (joiner ",") #:suffix (suffix ""))
  (string-join
   (map (λ (num)
          (format "word_~a ~a" num (or suffix "")))
        (range depth))
   joiner))

(define (make-word-cols depth)
  (make-word-query depth #:suffix "VARCHAR(32) REFERENCES words(word)"))

(define (make-word-col-names depth)
  (make-word-query depth))

(define (make-word-select num-words)
  (make-word-query num-words #:joiner " AND " #:suffix "= ?"))

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
  (query-exec db-con (format "CREATE TABLE markov(~a, frequency INTEGER DEFAULT 0, UNIQUE KEY words (~a))"
                             (make-word-cols depth)
                             (make-word-col-names depth))))

(define (word-generator)
"Generator to output one word at a time from the current
input port."
  (generator ()
    (yield "#_START")
    (let-values (((newlines line) (consume-blank-lines)))
      (let gen-loop ((newlines newlines)
                     (line line))
        (cond
          ((= newlines 1)
           (yield "#_BREAK"))
          ((> newlines 1)
           (begin (yield "#_END") (yield "#_START"))))
        (if (not line)
            (yield #f)
            (begin
              (for-each yield (string-split (string-replace line #rx"[^A-Za-z0-9_ .?!,;:'\"-]" "")))
              (let-values (((newlines line) (consume-blank-lines)))
                (if (eof-object? line)
                    (yield "#_END")
                    (begin
                      (yield "#_LINE_BREAK")
                      (gen-loop newlines line))))))))))

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
  (let* ((unique-words (get-unique-words word-hash))
         (word-insert-query (format "INSERT IGNORE INTO words VALUES ~a"
                                   (make-vals-template 1 (length unique-words))))
         (markov-insert-query (format (string-join (list
              "INSERT INTO markov"
              "VALUES ~a"
              "ON DUPLICATE KEY UPDATE frequency = frequency + VALUES(frequency)"))
                                      (make-vals-template (1+ depth) (hash-count word-hash)))))
    (apply query-exec (append (list db-con word-insert-query) unique-words))
    (apply query-exec (append (list db-con markov-insert-query) (get-word-insert-vals word-hash)))))

(define (get-in db-con words)
"Gets a value from the database using the given words."
  (let ((word-query (format "SELECT word_~a, SUM(frequency) FROM markov WHERE ~a GROUP BY word_~a"
                            (length words)
                            (make-word-select (length words))
                            (length words))))
    (apply query-rows (append (list db-con word-query) words))))

(define (train db-con depth cache-size)
"Modifies a word hash from the current input port."
  (let* ((g (word-generator))
         (word-hash (make-hash)))
    (let train-loop ((words (list (g))))
      (when (or (not (first words))
                (= (hash-count word-hash) cache-size))
        (begin (upsert-words db-con word-hash depth)
               (hash-clear! word-hash)))
      (cond
        ((and (> (length words) 1)
              (equal? "#_START" (first words))
              (equal? "#_END" (second words)))
         (train-loop '("#_START")))
        ((< (length words) depth)
         (train-loop (cons (g) words)))
        ((not (first words))
         #t)
        (else
         (begin
           (hash-update! word-hash (reverse words) 1+ 1)
           (train-loop (cons (g) (drop-right words 1)))))))))

(define (choose-weighted-word weighted-words)
"Picks a word at random from a weighted list of words."
  (if (not (empty? weighted-words))
      (let loop ((num (random (1+ (foldl + 0 (map (λ (vec) (vector-ref vec 1)) weighted-words)))))
                 (words weighted-words))
        (let ((word (vector-ref (car words) 0))
              (weight (vector-ref (car words) 1)))
          (if (>= weight num)
              word
              (loop (- num weight) (cdr words)))))
      #f))

(define (choose-word db-con words lower-threshold upper-threshold)
"Picks a word based on the words already we already have."
  (let gen-loop ((words words))
    (let ((weighted-words (get-in db-con words)))
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
    ((or "#_BREAK" "#_END")
     (string-append output-text "\n\n"))
    ("#_LINE_BREAK"
     (string-append output-text "\n"))
    ("#_START"
     output-text)
    (_
     (string-append output-text " " word))))

(define (generate db-con depth num-words lower-threshold upper-threshold (theme-words #f))
"Generates a number of words using a word hash and given
chain/state depth."
  (let ((first-pass
    (let gen-loop ((words '("#_START"))
                   (current-length 1))
      (let* ((ordered-words (reverse (take words (min depth current-length))))
             (word (choose-word db-con ordered-words lower-threshold upper-threshold)))
        (if (or (not word)
                (or (and (> current-length num-words)
                         (or (equal? word "#_END")
                             (equal? word "#_BREAK")))
                    (and (> current-length (* num-words 1.2))
                         (or (equal? word "#_END")
                             (equal? word "#_BREAK")
                             (equal? word "#_LINE_BREAK")))))
            (drop (reverse words) 1)
            (gen-loop (cons word words) (1+ current-length)))))))
    (foldl text-fold ""
               (if theme-words
                   (replace-words first-pass theme-words)
                   first-pass))))
