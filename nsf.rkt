#lang racket/base

(require scribble/base scribble/decode
         (except-in scribble/core paragraph)
         (rename-in scribble/doclang [#%module-begin -#%module-begin])
         scribble/private/defaults
         scribble/html-properties scribble/latex-properties
         setup/main-collects
         setup/collects
         (for-syntax racket/base
                     racket/syntax))

(provide (all-from-out scribble/base)
         (except-out (all-from-out scribble/doclang)
                     -#%module-begin)
         (rename-out [--#%module-begin #%module-begin]))

(define ((post-process [point "11pt"]) doc)
  (add-defaults doc
                (string-append
                 "\\documentclass["
                 point
                 "]{article}\n"
                 "\\usepackage[empty]{fullpage}\n"
                 "\\pagestyle{plain}\n"
                 "\\setlength{\footskip}{0.5in}\n"
                (collection-file-path "style.tex" "scribble" "nsf")
                null
                #f)))

(define-syntax (--#%module-begin stx)
  (syntax-case stx ()
    [(_ journal ?e ...)
     (with-syntax ([doc (format-id stx "doc")])
       (quasisyntax/loc stx
         (-#%module-begin doc (post-process '#,(syntax->datum #'journal)) () ?e ...)))]))

;; Reader configuration for #lang
(module reader scribble/base/reader
  scribble/nsf
  #:wrapper1 (lambda (t) (t)))
