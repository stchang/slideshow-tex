#lang setup/infotab
(define name "slideshow-tex")
(define scribblings '(("main.scrbl")))
(define categories '(misc))
(define repositories (list "4.x"))
(define primary-file 
  '("main.rkt"))
(define blurb
  (list '(div "Package to use LaTex code in Slideshow.")))
(define release-notes 
  (list
   '(div "Fix define-preamble to work with custom .sty files.")))