#lang info

(define collection 'multi)

(define deps '("base" "scribble-lib" "at-exp-lib"))
(define build-deps '("racket-doc" "scribble-doc"))

(define pkg-desc "A Scribble document class for NSF proposals")
(define pkg-authors '(chrdimo))

(define license
  'CC-BY-4.0)
