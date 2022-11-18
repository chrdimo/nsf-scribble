#lang racket/base

(require (except-in scribble/base author)
         scribble/decode
         racket/string
         scribble/core
         (rename-in scribble/doclang [#%module-begin -#%module-begin])
         scribble/private/defaults
         racket/list
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

(provide (all-from-out scribble/base)
         (except-out (all-from-out scribble/doclang)
                     -#%module-begin)
         (rename-out [--#%module-begin #%module-begin])
         author
         turn-page+reset
         chapter
         titled-para
         medskip)


(define tex-prelude
  #<<FORMAT
\PassOptionsToPackage{usenames,dvipsnames}{color}
\documentclass[11pt]{article}
\usepackage[empty]{fullpage}
\pagestyle{~a}
\setlength{\footskip}{0.5in}
\bibliographystyle{abbrvnat}
FORMAT
  )

(define ((post-process page-numbers?) doc)
  (add-defaults
   doc
   (string->bytes/utf-8 (format tex-prelude (if page-numbers? "plain" "empty")))
   (collection-file-path "style.tex" "scribble" "nsf")
   null
   #f))


(define-syntax (--#%module-begin stx)
  (syntax-parse stx
    [(_ . full-body)
     (define options '())
     (define body
       (let loop ([body #'full-body])
         (syntax-parse body
           #:literals (page-numbers)
           [() body]
           [(ws:string . body)
            #:fail-unless
            (or (regexp-match? #rx"^[ \t]*$" (syntax-e #'ws))
                (equal? (syntax-e #'ws) "\n"))
            "whitespace"
            (if (equal? (syntax-e #'ws) "\n")
                #'body
                (loop #'body))]
           [(page-numbers . body)
            (set! options (cons 'page-numbers options))
            (loop #'body)])))
     (with-syntax ([doc (format-id stx "doc")])
       (quasisyntax/loc stx
         (-#%module-begin doc
                          (post-process #,(and (member 'page-numbers options) #t))
                          ()
                          #,@body)))]))


(define (author names department university)
  (make-paragraph 
   (make-style 'author '(centered))
   (let* ([nl (make-element (make-style #f '(exact-chars)) "\\\\")]
          [spacing "1cm"]
          [spaces (make-element (make-style #f '(exact-chars)) (format "\\hspace{~a}" spacing))]
          [names
           (case (length names)
             [(2) (string-append (car names) " and " (cadr names))]
             [else (add-between names spaces)])])
     (list names nl department nl university))))


(define (turn-page+reset)
 (make-element (make-style #f '(exact-chars)) "\\nsfturnpageandreset"))

(define (medskip)
 (make-element (make-style #f '(exact-chars)) "\\medskip"))

(define (chapter proposal-title chapter-title)
 (make-multiarg-element "nsfchapter"
                        (list (decode-content (list proposal-title))
                              (decode-content (list chapter-title)))))     
  
(define (titled-para title)
 (make-multiarg-element "nsftitledpara"
                        (list (decode-content (list title)))))     

(define-syntax (page-numbers stx)
  (raise-syntax-error #f
                      "option must appear on the same line as `#lang scribble/nsf'"
                      stx))
(provide page-numbers)

(module reader scribble/base/reader
scribble/nsf
#:wrapper1 (lambda (t) (t)))