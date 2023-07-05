#lang info
(define collection "dexter-quickscripts")
(define deps '("gui-lib"
               "quickscript"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/dexter-quickscripts.scrbl" () ("DrRacket Quickscripts"))))
(define pkg-desc "A collection of quick scripts for Quickscripts.")
(define version "1.0")
(define license '(MIT))
