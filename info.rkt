#lang info
(define collection "binary-matcher")
(define deps '("base" "extra-srfi-libs" "soup-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "net-ip-lib"))
(define scribblings '(("scribblings/binary-matcher.scrbl" () ("Parsing Libraries"))))
(define pkg-desc "Match expander for destructuring bytestrings")
(define version "0.0")
(define pkg-authors '(shawnw))
(define license '(Apache-2.0 OR MIT))
