#lang scribble/doc

@(require scribble/manual
          planet/scribble
	  (for-label (file "main.rkt") 
                     ;(this-package-in main)
                     racket
                     slideshow
                     ))

@title{LaTeX for Slideshow}

@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

This package provides a way to use LaTeX code directly in Slideshow.

@defmodule[(planet stchang/slideshow-tex)]

@section{Requirements}

This package requires the following to be installed: 

@itemlist{@item{LaTeX}@item{an updated @tt{geometry.sty} that includes @tt{\newgeometry}. Check @link["http://ctan.org/tex-archive/macros/latex/contrib/geometry/"]{here} to download an updated @tt{geometry.sty} and for install directions.}@item{ImageMagick}}

@section{How to Use}

@defproc[(tex [str string?] ...) pict?]{Converts input LaTeX code into @racket[pict] for use in Slideshow.}

Conversion steps:

1) Input strings inserted into LaTeX document (in math mode) and compiled to pdf.

(Temporary files are created in the directory @tt{<tmpdir>/slideshow-texfiles/} where @tt{<tmpdir>} is the result of @tt{(@racket[find-system-path] 'temp-dir)}.
Temporary files are named @tt{texfile<#>.<ext>} where @tt{<#>} is the @racket[equal-hash-code] of the input string and @tt{<ext>} is the appropriate extension (ie, tex, aux, etc.).)

2) pdf file converted to png file using ImageMagick.

(All temporary files, except png files, are deleted after the conversion is done.)

3) Slideshow @racket[pict] returned using @racket[bitmap].

Results of compilation (ie, the png files) are cached. This function first checks for an existing png file (identified with the @racket[equal-hash-code] hash) and uses instead of recompiling every time.

Example: 

@verbatim{> (tex "\\lambda x.x")}

@image{img/example1.png}

@defproc[(define-preamble pa) void?]{Adds a custom preamble. Use this to import any extra latex package. Each call overwrites the previously defined preamble.}

@defproc[(tex-remove-all-cached-files) boolean?]{Removes all cached files in @tt{<tmpdir>/slideshow-texfiles/} where @tt{<tmpdir>} is equal to @tt{(@racket[find-system-path] 'temp-dir)}.}