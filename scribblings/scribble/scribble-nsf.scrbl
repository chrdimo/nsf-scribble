#lang scribble/manual
@(require (for-label scribble/nsf (only-in racket/base define) scriblib/autobib))

@title{A Scribble Document Style for NSF Proposals}
@author{Christos Dimoulas and Robby Findler}

@defmodulelang[scribble/nsf]{The
 @racketmodname[scribble/nsf] language is like
 @racketmodname[scribble/base], but configured with LaTeX
 style defaults to conform to the
 @link["https://www.nsf.gov/publications/pub_summ.jsp?ods_key=papp"]{NSF
  PAPPG}.}

Here's some boilerplate to get you started:

@codeblock[#:indent 2]|{
  #lang scribble/nsf
  @(define proposal-title "SHF: Small: Doing More with Less")
  @(define-cite ~cite citet generate-bibliography #:style number-style)
  @(define plt-tr1
    (make-bib #:title    "Reference: Racket"
              #:author   (authors "Matthew Flatt" "PLT")
              #:date     "2010"
              #:location (techrpt-location #:institution "PLT Design Inc."
                                           #:number "PLT-TR-2010-1")
              #:url      "https://racket-lang.org/tr1/"))  
  @title{@proposal-title}
  @chapter[@proposal-title]{Project Summary}
  @centered{@bold{Overview}}
  This is going to be great. Research, research, research.

  @bold{Keywords:} great work, just great.

  @centered{@bold{Intellectual Merit}}

  @centered{@bold{Broader Impacts}}

  @chapter[@proposal-title]{Project Description}

  @section{Introduction}

  Stuff goes here; this work builds on Racket@~cite[plt-tr1].

  @generate-bibliography[#:sec-title "References Cited"]
}|

@defidform[page-numbers]{
Enables page numbers. Must be used on the same
line as @hash-lang[], with only whitespace between
@racketmodname[scribble/nsf] and the format name, i.e.:

@codeblock[#:indent 2]|{
  #lang scribble/nsf @page-numbers
}|
}

@defproc[(author [names (listof string?)]
                 [department content?]
                 [university content?])
         block?]{
                 
 Adds the authors given by @racket[names],
 @racket[department], and @racket[university] to the
 document.

}

@defproc[(turn-page+reset) element?]{
 Returns an element that ends the current page.
}

@defproc[(chapter [proposal-title pre-content?]
                  [chapter-title pre-content?]) element?]{

 Ends the current page and adds a heading based on
 @racket[proposal-title] and @racket[chapter-title]
 on the page just after.
}

@defproc[(titled-para [title pre-content?]) element?]{

 Adds some vertical space and starts a new paragraph without
 any indentation, but with @racket[title] (in bold).

}

@defproc[(medskip) element?]{

 Returns an element that adds some vertical space using
 LaTeX's @tt{\medskip}.

}
