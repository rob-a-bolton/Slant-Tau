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

#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco doc <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
