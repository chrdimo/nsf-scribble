#lang racket/base

(require (except-in scribble/base author)
         scribble/decode
         scribble/core
         (rename-in scribble/doclang [#%module-begin -#%module-begin])
         scribble/private/defaults
         racket/list
         (for-syntax racket/base
                     racket/syntax))

(provide (all-from-out scribble/base)
         (except-out (all-from-out scribble/doclang)
                     -#%module-begin)
         (rename-out [--#%module-begin #%module-begin])
         author
         turn-page+reset
         chapter
         titled-para)


(define ((post-process) doc)
  (add-defaults doc
                (string->bytes/utf-8 #<<FORMAT
\PassOptionsToPackage{usenames,dvipsnames}{color}
\documentclass[11pt]{article}
\usepackage[empty]{fullpage}
\pagestyle{plain}
\setlength{\footskip}{0.5in}
\bibliographystyle{abbrvnat}
FORMAT
                                     )
                (collection-file-path "style.tex" "scribble" "nsf")
                null
                #f))


(define-syntax (--#%module-begin stx)
  (syntax-case stx ()
    [(_ ?e ...)
     (with-syntax ([doc (format-id stx "doc")])
       (quasisyntax/loc stx
         (-#%module-begin doc (post-process) () ?e ...)))]))



(define (author names department university)
  (make-paragraph 
   (make-style 'author null)
   (let ([nl (make-element (make-style #f '(exact-chars)) "\\\\")]
         [names
          (case (length names)
            [(2) (string-append (car names) " and " (cadr names))]
            [else (add-between names " ")])])
     (list names nl department nl university))))


(define (turn-page+reset)
 (make-element (make-style #f '(exact-chars)) "\\nsfturnpageandreset"))

(define (chapter proposal-title chapter-title)
 (make-multiarg-element "nsfchapter"
                        (list (decode-content (list proposal-title))
                              (decode-content (list chapter-title)))))     
  
(define (titled-para title . content)
  (make-multiarg-element "nsftitledpara"
                        (list (decode-content (list title))
                              (decode-content content))))


(module reader scribble/base/reader
scribble/nsf
#:wrapper1 (lambda (t) (t)))