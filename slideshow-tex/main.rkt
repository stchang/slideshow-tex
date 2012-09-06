#lang racket

(require racket/system)
(require slideshow)

(provide (rename-out [tex-math tex]) tex-remove-all-cached-files define-preamble)

(define tex-dir (string-append (path->string (find-system-path 'temp-dir)) 
                               "/slideshow-texfiles/"))

;; -> string
(define (get-filenames [str #f])
  (define preprefix "texfile")
  (define prefix
    (if str
        (string-append preprefix (number->string (equal-hash-code str)))
        (symbol->string (gensym preprefix))))
  (values prefix
          (string-append prefix ".tex") 
          (string-append prefix ".pdf")
          (string-append prefix ".png")))

(define (tex-math . strs)
  (tex (string-append "$" (apply string-append strs) "$")))
                  

; required preamble
(define preamble "\\usepackage[pass]{geometry}\n")

; customizable preamble (default value is for backwards compatibility)
(define custom-preamble
  (string-append
   "\\usepackage{color}\n"
   "\\definecolor{grayed}{gray}{0.4}\n"
   "\\definecolor{lightgrayed}{gray}{0.8}\n"
   "\\definecolor{black}{gray}{0}\n"
   "\\definecolor{white}{gray}{1}\n"))
   
; sets the customizable preamble
(define (define-preamble pa) (set! custom-preamble pa))

(define (tex . strs)
  (define str (apply string-append strs))
  (define-values (fileroot texfile pdffile pngfile) (get-filenames str))
  (define saved-cwd (current-directory))
  (define saved-texinputs (getenv "TEXINPUTS"))
  (if (file-exists? (string-append tex-dir pngfile))
      (bitmap (string-append tex-dir pngfile))
      (begin
        (unless (directory-exists? tex-dir) (make-directory tex-dir))
        (current-directory tex-dir)
        (if saved-texinputs
            (putenv "TEXINPUTS" (string-append (path->string saved-cwd) ":"
                                               saved-texinputs))
            (putenv "TEXINPUTS" (path->string saved-cwd)))
        (let ([o (open-output-file texfile #:mode 'binary #:exists 'replace)])
          (display (string-append
                    "\\documentclass{article}\n"
                    preamble
                    custom-preamble
                    "\\begin{document}\n"
                    "\\newbox\\mycontent\\savebox\\mycontent{" str "}\n"
                    "\\pdfpageheight=\\dimexpr\\dp\\mycontent+\\ht\\mycontent+6pt\n"
                    "\\pdfpagewidth=\\wd\\mycontent\n"
                    "\\newgeometry{vmargin=0pt,hmargin=0pt}\n"
                    "\\noindent\\makebox{\\usebox\\mycontent}\n"
                    "\\end{document}\n")
                   o)
          (close-output-port o)
          (system (string-append "pdflatex" " " texfile))
          (system (string-append "convert -density 300x300 " pdffile " " pngfile))
          (begin0
            (bitmap pngfile)
            (for-each delete-file
                      (list (string-append fileroot ".aux")
                            (string-append fileroot ".log")
                            pdffile
                            texfile))
            (current-directory saved-cwd)
            (if saved-texinputs
                (putenv "TEXINPUTS" saved-texinputs)
                (putenv "TEXINPUTS" ""))
            )))))

(define (tex-remove-all-cached-files)
  (system (string-append "rm -Rf " tex-dir)))