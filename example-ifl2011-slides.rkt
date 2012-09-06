#lang at-exp slideshow

(require scheme/gui) ; for font% class
(require (for-syntax racket/list)) ; take and drop

(require (planet stchang/slideshow-tex))
#;(require "slideshow-tex/main.rkt")


(define MAIN-FONT-FACE "CMU Sans Serif")
(define TT-FONT-FACE "CMU Typewriter Text")

(define (make-main-font size)
  (make-object font% size MAIN-FONT-FACE 'swiss 'normal 'normal #f 'smoothed))
(define (make-main-font-italic size)
  (make-object font% size MAIN-FONT-FACE 'swiss 'italic 'normal #f 'smoothed))
(define (make-main-font-bold size)
  (make-object font% size MAIN-FONT-FACE 'swiss 'normal 'bold #f 'smoothed))

(define (make-tt-font size)
  (make-object font% size TT-FONT-FACE 'modern 'normal 'light #f 'smoothed))
(define (make-tt-font-bold size)
  (make-object font% size TT-FONT-FACE 'modern 'normal 'bold #f 'smoothed))

(define MAIN-FONT-SIZE 32)
(define MAIN-FONT-SIZE-LARGE 40)
(define MAIN-FONT-SIZE-SMALL 24)
(define MAIN-FONT-SIZE-SMALLER 18)
(define MAIN-FONT-SIZE-SMALLERER 14)

(define TT-FONT-SIZE 32)
(define TT-FONT-SIZE-SMALL 24)
(define TT-FONT-SIZE-SMALLER 18)

(current-main-font (make-main-font MAIN-FONT-SIZE))

;; If factor < 1, the color is darkened 
;; by multiplying the RGB components by the factor. 
;; If factor > 1, the color is lightened 
;; by dividing the gap between the RGB components and 255 by the factor.
(define (gray txt-pict factor)
  (colorize txt-pict (scale-color factor "black")))

;;-----------------------------------------------------------------------------
;; override slideshow text fns to accept additional font-size param
;; ----------------------------------------------------------------------------
(define (t txt [size MAIN-FONT-SIZE] [angle 0])
  (text txt (make-main-font size) size angle))
(define (t-sm txt [angle 0])
  (t txt MAIN-FONT-SIZE-SMALL angle))
(define (t-lg txt [angle 0])
  (t txt MAIN-FONT-SIZE-LARGE angle))
(define (t-smm txt [angle 0])
  (t txt MAIN-FONT-SIZE-SMALLER angle))
(define (t-smmm txt [angle 0])
  (t txt MAIN-FONT-SIZE-SMALLERER angle))
(define (it txt [size MAIN-FONT-SIZE])
  (text txt (make-main-font-italic size)))
(define (it-sm txt)
  (it txt MAIN-FONT-SIZE-SMALL))
(define (it-smm txt)
  (it txt MAIN-FONT-SIZE-SMALLER))
(define (bt txt [size TT-FONT-SIZE])
  (text txt (make-main-font-bold size)))
(define (bt-sm txt)
  (bt txt MAIN-FONT-SIZE-SMALL))
(define (bt-smm txt)
  (bt txt MAIN-FONT-SIZE-SMALLER))
(define (tt txt [size TT-FONT-SIZE])
  (text txt (make-tt-font size)))
(define (tt-sm txt)
  (gray (tt txt TT-FONT-SIZE-SMALL) 1.5))
(define (btt txt [size TT-FONT-SIZE])
  (text txt (make-tt-font-bold size)))
(define (btt-sm txt [size TT-FONT-SIZE-SMALL])
  (btt txt size))
(define (btt-smm txt [size TT-FONT-SIZE-SMALLER])
  (btt txt size))

(define (underline txt-pict)
  (refocus (vc-append txt-pict (hline (pict-width txt-pict) 5))
           txt-pict))

(define (boxed txt-pict)
  (cc-superimpose txt-pict 
                  (rectangle (* (pict-width txt-pict) 1.1) (pict-height txt-pict))))



;; citation : String -> Pict
;; Adds brackets to given string and converts to pict of font-size 24
(define (citation . ref-strs)
  (apply
   vc-append
   (let loop ([lst (cons (string-append "[" (car ref-strs)) (cdr ref-strs))])
     (if (null? (cdr lst))
         (list (gray (t-smm (string-append (car lst) "]")) 1.3))
         (cons (gray (t-smm (car lst)) 1.3)
               (loop (cdr lst)))))))


(define-for-syntax txt-fns 
  (list #'t #'t-sm #'t-smm #'bt #'bt-sm #'bt-smm #'it #'it-sm #'it-smmm))

;; gets name of txt-fn used, 
;; or first txt-fn used (in the the case of a grouping)
(define-for-syntax (get-fn stx)
  (syntax-case stx ()
    [(f x ...)
     (for/or ([fn (in-list txt-fns)])
       (free-identifier=? fn #'f))
     #'f]
    [(combiner (f x ...) rest ...)
     (for/or ([fn (in-list txt-fns)])
       (free-identifier=? fn #'f))
     #'f]
    [_ #f]
    ))

(define-syntax (bulleted-list stx)
  (syntax-case stx ()
    [(_ bullet items ...)
     ; check if #'bullet == 'numbered
     (and (eq? (syntax-e (cadr (syntax-e #'bullet))) 'numbered))
     (let ([make-num-bullet (λ (n) (string-append (number->string n) ")"))])
       (with-syntax 
           ([(new-items ...)
             (for/list ([x (in-list (syntax->list #'(items ...)))]
                        [i (in-naturals 1)])
               #`(hbl-append 10 (#,(get-fn x) #,(make-num-bullet i)) #,x))])
         #'(vl-append new-items ...)))]
    [(_ bul items ...)
     (with-syntax 
         ([(new-items ...)
           (for/list ([x (in-list (syntax->list #'(items ...)))])
             #`(hbl-append 10 bul #,x))])
     #'(vl-append new-items ...))]))
                    
(define-for-syntax (split-list lst n)
  (list (take lst n) (drop lst n)))

(define-for-syntax (add-ghost stx) #`(ghost #,stx))

;; (bullet-list-alts #:gap Number
;;                   #:combiner Function (ie vl-append, etc)
;;                   #:bullet String  (will use same txt-fn as in list-items ...)
;;                              or
;;                          'numbered (produces numbered list)
;;                   list-items ...)
(define-syntax (bulleted-list-alts stx)
  (let*-values
      ([(gap rest1) ; ----- extract gap -----
        (syntax-case stx ()
          [(f #:gap gap . rest)
           (eq? (syntax->datum #'#:gap) '#:gap)
           (values (syntax-e #'gap) #'rest)]
          [_ (values 0 (cdr (syntax-e stx)))])]
       [(combiner rest2) ; ----- extract combiner -----
        (syntax-case rest1 ()
          [(#:combiner combiner . rest)
           (eq? (syntax->datum #'#:combiner) '#:combiner)
           (values #'combiner #'rest)]
          [_ (values #'vl-append rest1)])]
       [(bulleted-items)
        (syntax-case rest2 ()
          [(#:bullet bullet items ...) ; ----- non-numbered bullets -----
           (and (eq? (syntax->datum #'#:bullet) '#:bullet)
                (string? (syntax-e #'bullet)))
           (for/list ([x (in-list (syntax->list #'(items ...)))])
             #`(htl-append 10 (#,(get-fn x) bullet) #,x))]
          [(#:bullet bullet items ...) ; ----- numbered bullets -----
           (and (eq? (syntax->datum #'#:bullet) '#:bullet)
                (eq? (syntax-e (cadr (syntax-e #'bullet))) 'numbered))
           (let ([make-num-bullet (λ (n) (string-append (number->string n) ")"))])
             (for/list ([x (in-list (syntax->list #'(items ...)))]
                        [i (in-naturals 1)])
               #`(htl-append 10 (#,(get-fn x) #,(make-num-bullet i)) #,x)))]
          [(items ...)                 ; ----- no bullet -----
           (syntax->list #'(items ...))])])
    (let* ([stages
            (for/list ([i (in-range (add1 (length bulleted-items)))])
              (split-list bulleted-items i))]
           [stages-noghostonly (map first stages)]
           [stages-ghostedonly
            (for/list ([items-toghost-one-step (in-list (map second stages))])
              (map add-ghost items-toghost-one-step))]
           [new-stages (map append stages-noghostonly stages-ghostedonly)])
      (with-syntax ([(stages-ready ...)
                     (for/list ([one-stage (in-list new-stages)])
                       (with-syntax ([(new-items ...) one-stage])
                         #`(list (#,combiner #,gap new-items ...))))])
        #'(list stages-ready ...)))))

(current-para-width 900)

(define-preamble
  (string-append
   "\\usepackage{mysty}\n"
   "\\usepackage{color}\n"
   "\\definecolor{grayed}{gray}{0.4}\n"
   "\\definecolor{lightgrayed}{gray}{0.8}\n"
   "\\definecolor{black}{gray}{0}\n"
   "\\definecolor{white}{gray}{1}\n"
   ))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; Begin slides
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(slide
 (comment "blank -- testing")
 @tex{\newlam})

(slide
 (comment "")
 #:name "Title"
 (colorize (t "From Stack Traces to Lazy Rewriting Sequences") "blue")
 (hbl-append (bt-smm "Stephen Chang") (t-smm ", Eli Barzilay, John Clements*, Matthias Felleisen"))
 (t-smm "Northeastern University")
 (t-smm "*California Polytechnic State University")
 (t-sm "10/5/2011"))


(slide
 (comment 
  "As users of lazy languages know ..\n\n"
  "Trying to debug your program can be really really frustrating sometimes.\n\n"
  "BUT\n\n"
  "Things ARE getting better."
  )
  #:name "Debugging lazy programs is hard."
  (colorize (t-lg "Debugging lazy programs is hard.") "red"))



(slide
 (comment
  "Here's a list of some recent tools developed for Haskell, the most widely used lazy language.\n\n"
  "Seeing all these tools, it's clear that lazy debugging is improving.\n\n"
  "However ...\n\n"
  "From the recent State of Haskell survey, 50% of respondents thought that tool selection was the biggest weakness. So it seems like many users are still unsatisfied.\n\n"
  "And if even Haskell users are unhappy about their tools, it means that lazy debugging in general still has room for improvement.")
 #:name "Existing Haskell tools"
 (t-smm "Freja (Nilsson and Fritzson 1992)")
 (t-smm "Hat (Sparud and Runciman, 1997)")
 (t-smm "Buddha (Pope, 1998)")
 (t-smm "HOOD (Gill, 2000)")
 (t-smm "New Hat (Wallace et al., 2001)")
 (t-smm "HsDebug (Ennals and Peyton Jones, 2003)")
 (t-smm "Rectus (Murk and Kolmoldin, 2006)")
 (t-smm "GHCi debugger (Marlow et al., 2007)")
 (t-smm "StackTrace (Allwood et al., 2009)")
 (blank)
 'next
 (vc-append
  (it-sm "What do you think is Haskell's most glaring")
  (hbl-append (it-sm "weakness / blind spot / problem? ") (citation "Tibell, Knowlson 2011")))
 (colorize (t-lg "Inadequate Tools (50%)") "red")
 )


(define-syntax (make-quote stx)
  (syntax-case stx ()
    [(_ quote-first quote ... quote-last years)
     (with-syntax 
         ([(rendered-quote ...)
           (for/list ([s (in-list (syntax->list #'(quote ...)))])
             #`(it-sm #,s))])
       #`(vc-append 
          (it-sm #,(string-append "\"" (syntax-e #'quote-first)))
          rendered-quote ... 
          (hbl-append (it-sm #,(string-append (syntax-e #'quote-last) "\""))
                      (gray (it-sm #,(string-append " " (syntax-e #'years))) 1.5))))]
    [(_ quote-single years)
     #`(hbl-append (it-sm (string-append "\"" (syntax-e #'quote-single) "\""))
                   (gray (it-sm #,(string-append " " (syntax-e #'years))) 1.5))]))

#;(slide
 (comment "")
 #:title "Quotes on Laziness [sic]"
 'alts
 (bulleted-list-alts
  #:gap 40
  #:combiner vc-append
  (make-quote "Laziness is hard to come to grips with." 
              "It's powerful and good, but it also causes strange problems"
              "that a beginner often cannot diagnose."
              "(months)")
  (make-quote "It's hard to understand exactly how much work"
              "is being done due to lazyness."
              "(1yr)")
  (make-quote "The biggest drawback is definitely"
              "reasoning about strictness / laziness." 
              "(3yrs)")
  (make-quote "The fact that reasoning about the runtime behavior is hard,"
              "because of lazyness." 
              "(4yrs)")
  ))


(slide
 (comment 
  "Here are some direct quotes from the survey.\n\n"
  "quote1\n\n"
  "I've also added the respondent's experience in parentheses.\n\n"
  "quote2-5\n\n"
  "As you can see, it's not just the beginners that struggle with laziness. A wide spectrum of experience is represented in these quotes.\n\n"
  "From the responses, it seems like there's opportunity to explore alternative, \"step-based\" tools for lazy languages.\n\n")
 #:name "Quotes on Debugging [sic]"
 (citation "State of Haskell Survey 2011")
 'alts
 (bulleted-list-alts
  #:gap 40
  #:combiner vc-append
  (make-quote "A debugger adjusted to the complexity of debugging"
              "lazily evaluated structures." 
              "(weeks)")
  (make-quote "Laziness is hard to come to grips with." 
              "It's powerful and good, but it also causes strange problems"
              "that a beginner often cannot diagnose."
              "(months)")
  (make-quote "I think that a good debugger that lets me step through a program "
              "/quickly and comfortably/ would be a great help." 
              "(1yr)")
  (make-quote "I'd love to see some debugging"
              "(~step by step evaluation/run tracing) support."
              "(2yrs)")
  (make-quote "Debugging lazy code" 
              "(4 yrs)"))
 'next
 (blank)
 (blank)
 (boxed (hbl-append (t "Better lazy ") (bt "step-based") (t " tools are needed.")))
  )

(slide
 (comment 
  "But before we can build a step-based tool, we first have to define what a step is.\n\n"
  "In imperative languages, it's obvious. But in functional languages, and especially lazy languages, it's less clear how to define a step.\n\n"
  "There HAVE been attempts to build step-based lazy debuggers, so I'm going to review some the previous work.\n\n"
  "In the process, I'm also going to collect ideas for things we might want in an alternative tool.\n\n")
 #:name "What's a step?"
 (colorize (t-lg "What's a \"step\"?") "blue"))

(slide
 (comment 
  "Ennals and Peyton Jones created HsDebug, a step-based debugger for Haskell.\n\n"
  "It's based on optimistic evaluation, where expressions are evaluated eagerly by default.\n\n"
  "It uses special mechanisms to handle things like non-termination or errors, so that you get the same result as a lazy program.\n\n"
  "However, the required changes to the evaluation machinery were ultimately deemed too difficult to implement, so optimistic evaluation is not used much today.")
 #:name "Ennals and Peyton Jones"
 (colorize (t "HsDebug") "blue")
 (citation "Ennals and Peyton Jones 2003")
 (blank)
 'next
 (item "Evaluate expressions optimistically.")
 'next
 (item "To preserve lazy behavior, handle special cases:")
 (subitem "non-termination")
 (subitem "errors")
 'next
 (item "Too difficult to implement.")
 )

(define (make-idea-title num)
  (colorize (it (string-append "Idea #" (number->string num) ":"))
            (scale-color 0.7 "yellow")))
(slide
 (comment 
  "This brings us to the first idea for an alternative, step-based lazy debugger. The debugger should not change the way a program runs. This means that it should preserve laziness.")
 #:name "Idea #1"
 #;(colorize (it "Idea #1:") (scale-color 0.7 "yellow"))
 (make-idea-title 1)
 (t "Debugger shouldn't change the program evaluation model." 
    (- MAIN-FONT-SIZE 2)))

(define ghci-example-line1 (tt-sm "test1 x y = (test2 y) + x"))
(define ghci-example-line1-gray (gray (tt-sm "test1 x y = (test2 y) + x") 2))
(define ghci-example-line1-bold (btt-sm "test1 x y = (test2 y) + x"))
(define ghci-example-line2 (tt-sm "test2 x = x * 2"))
(define ghci-example-line2-gray (gray (tt-sm "test2 x = x * 2") 2))
(define ghci-example-line2-bold (btt-sm "test2 x = x * 2"))
(define ghci-example-line3 (tt-sm "test3 x = x + 1"))
(define ghci-example-line3-gray (gray (tt-sm "test3 x = x + 1") 2))
(define ghci-example-line3-bold (btt-sm "test3 x = x + 1"))
(define ghci-example-line4 (tt-sm "main = print $ test1 (1 + 2) (test3 (3 + 4))"))
(define ghci-example-line4-gray (gray (tt-sm "main = print $ test1 (1 + 2) (test3 (3 + 4))") 2))
(define ghci-example-line4-bold (btt-sm "main = print $ test1 (1 + 2) (test3 (3 + 4))"))
(slide
 (comment 
  "Simon Marlow and his co-authors agree with this idea and incorporate it into their step-based debugger for GHCi. Their tool shows the effects of laziness when evaluating a program.\n\n"
  "However, both users of the tool and the authors themselves acknowledge that the generated steps can be hard to follow, for example, when evaluation jumps back to a call site to force arguments.\n\n"
  "Here is a small example of this. I've collected the ouput from single-stepping through this program. The code highlighted in black is currently being evaluated. First, we have a call to test1. In the body of test1, there's a call to test2. test2 needs its x argument, which is a call to test3. Test3 then needs its x argument, so we jump back out and evaluate it. This finishes the call to test2 in the body of test1, which then needs it's x, so we jump to evaluate test1's other argument.\n\n"
  "The problem here is that you have to keep a lot of the context in your head, like remembering which variables correspond to what arguments. At best you have to query the tool for this information. For example, in the last step, we were a few nested calls down and may not have remembered we needed the x argument to test1, so the jump could have been confusing.\n\n"
  "The general problem is that you're seeing the low-level semantics of the underlying implementation, which programmers may not be familiar with.\n\n"
  "If the programmer has difficulty predicting what the next step should be, then debugging becomes difficult.\n\n")
 #:name "Marlow et al."
 (colorize (t "GHCi Debugger") "blue")
 (citation "Marlow et al. 2007")
 'next
 (item (t-sm "Shows the effects of laziness."))
 'next
 (item (t-sm "\"having execution jump around can be distracting and confusing\""))
 'alts
 (list
  (list)
  (list
   (vl-append
    ghci-example-line1
    ghci-example-line2
    ghci-example-line3
    (hbl-append (tt-sm "main = ")
                (btt-sm "print $ test1 (1 + 2) (test3 (3 + 4))"))))
  (list
   (vl-append
    ghci-example-line1
    ghci-example-line2
    ghci-example-line3
    (hbl-append (tt-sm "main = print $ ")
                (btt-sm "test1 (1 + 2) (test3 (3 + 4))"))))
  (list
   (vl-append
    ghci-example-line1
    ghci-example-line2
    ghci-example-line3
    (hbl-append (tt-sm "main = print $ ")
                (btt-sm "test1 (1 + 2)")
                (tt-sm " (test3 (3 + 4))"))))
  (list
   (vl-append
    ghci-example-line1-bold
    ghci-example-line2
    ghci-example-line3
    ghci-example-line4))
  (list
   (vl-append
    (hbl-append (tt-sm "test1 x y = ")
                (btt-sm "(test2 y) + x"))
    ghci-example-line2
    ghci-example-line3
    ghci-example-line4))
  (list
   (vl-append
    (hbl-append (tt-sm "test1 x y = ")
                (btt-sm "(test2 y)")
                (tt-sm " + x"))
    ghci-example-line2
    ghci-example-line3
    ghci-example-line4))  
  (list
   (vl-append
    ghci-example-line1
    ghci-example-line2-bold
    ghci-example-line3
    ghci-example-line4))
  (list
   (vl-append
    ghci-example-line1
    (hbl-append (tt-sm "test2 x = ")
                (btt-sm "x * 2"))
    ghci-example-line3
    ghci-example-line4))
  (list
   (vl-append
    ghci-example-line1
    ghci-example-line2
    ghci-example-line3
    (hbl-append (tt-sm "main = print $ test1 (1 + 2) ")
                (btt-sm "(test3 (3 + 4))"))))
  (list
   (vl-append
    ghci-example-line1
    ghci-example-line2
    ghci-example-line3-bold
    ghci-example-line4))  
  (list
   (vl-append
    ghci-example-line1
    ghci-example-line2
    (hbl-append (tt-sm "test3 x = ")
                (btt-sm "x + 1"))
    ghci-example-line4))  
  (list
   (vl-append
    ghci-example-line1
    ghci-example-line2
    ghci-example-line3
    (hbl-append (tt-sm "main = print $ test1 (1 + 2) (test3 ")
                (btt-sm "(3 + 4)")
                (tt-sm ")"))))
  (list
   (vl-append
    ghci-example-line1
    ghci-example-line2
    ghci-example-line3
    (hbl-append (tt-sm "main = print $ test1 ")
                (btt-sm "(1 + 2)")
                (tt-sm " (test3 (3 + 4))")))) 
  )
 'next
 (item (t-sm "Step semantics correspond to low-level implementation")
       (t-sm "-- unfamiliar to programmers."))
 )

#;(slide
 (comment "The survey respondent echoes the previous point, that reasoning about lazy programs is especially difficult if you dont even know the underlying semantics.\n\n")
 (citation "State of Haskell, 2011 Survey")
 (make-quote "Even lazy evaluation (i.e. by graph reduction)"
             "is implementation specific." 
             "(2yrs)"))


(slide
 (comment 
  "This leads to idea #2. A step-based debugger for lazy languages should use a high-level semantics that all programmers are familiar with.\n\n"
  "This would greatly facilitate the ease-of-use of the tool.")
 #:name "Idea #2"
 (make-idea-title 2)
 (t "Debugger should use a high-level semantics familiar to programmers."
    (add1 MAIN-FONT-SIZE-SMALL)))

(define gibbons-example-1 
  (scale @tex{(\lambda x.\lambda y.x+y)\;(1+2)\;(3+4)} 0.8))
(define gibbons-example-2 
  (scale @tex{\rightarrow(\lambda x.\lambda y.x+y)\;3\;(3+4)} 0.8))
(define gibbons-example-3 
  (scale @tex{\ldots\rightarrow(\lambda x.\lambda y.3+y)\;3\;(3+4)} 0.8))
(define gibbons-example-4
  (let ([pic 
         (scale 
          @tex{\rightarrow (\lambda x.(\lambda y.3+y)\;(3 + 4))\;3\;\textcolor{lightgrayed}{(3+4)}} 
          0.8)]
        [b (blank 150 30)])
    (vl-append 
     pic 
     (vr-append -10
      (blank (- (pict-width pic) 50) 0)
      (cc-superimpose
       (pin-line b b rt-find b rb-find #:line-width 4 #:color "gray")
       (pin-line b b rb-find b lb-find #:line-width 4 #:color "gray")
       (pin-arrow-line 20 b b lb-find b lt-find #:line-width 4 #:color "gray"))))))
(define gibbons-example-5
  (scale @tex{\rightarrow (\lambda x.(\lambda y.3+y)\;7)\;3} 0.8))
(define gibbons-example-6
  (scale @tex{\ldots\rightarrow (\lambda x.(\lambda y.3+7)\;7)\;3} 0.8))
(define gibbons-example-7
  (scale @tex{\ldots\rightarrow (\lambda x.(\lambda y.10)\;7)\;3} 0.8))

(slide
 (comment 
  "Gibbons and Wansbrough, from Australia, implemented a lazy stepping tool based on the call-by-need lambda calculus of Ariola et al.\n\n"
  "However, even though the tool is based on a high-level semantics, which is what we said we want, it's still confusing to follow.\n\n"
  "The fault here is in the rules of this particular semantics. It turns out that not all high-level semantics are suitable for use in debuggers.\n\n"
  "One confusing aspect is that reductions in the call-by-need lambda calculus never discharge arguments; they're carried around all the way to the end. This can make reduction sequences look very cluttered.\n\n"
  "Take this program for example. Evaluation finds the x in demand, so its argument 1+2 is evaluated. After the argument is evaluated, it's substituted for the x. But after that, the 3 doesn't go away.\n\n"
  "A second confusing aspect is the calculus has many extra reductions that have no corresponding behavior in actual lazy languages.\n\n"
  "To continue evaluation of the example program, we must first rescope the x binding, which doesnt happen in actual lazy language. So the (3+4) argument gets pulled under the outer lambda. Only then can computation continue.\n\n"
  "And you can see, again, the arguments hang around, even when evaluation is finished.\n\n")
 #:name "Gibbons and Wansbrough"
 (colorize (t "A reduction semantics-based tool") "blue")
 (citation "Gibbons and Wansbrough 1996, Ariola et al. 1995")
 (blank)
 'next
 (item "Persistent arguments clutter reductions.")
 'alts
 (list
  (list)
  (list (vr-append gibbons-example-1 (ghost gibbons-example-3)))
  (list (vr-append gibbons-example-1 (rc-superimpose gibbons-example-2
                                                     (ghost gibbons-example-3))))
  (list (vr-append gibbons-example-1 gibbons-example-3))
  )
 'next
 (item "Extraneous reductions.")
 'alts
 (list
  (list)
  (list gibbons-example-4))
 'next
 'alts
 (list
  (list (rc-superimpose gibbons-example-5 (ghost gibbons-example-6)))
  (list gibbons-example-6)
  (list (rc-superimpose gibbons-example-7 (ghost gibbons-example-6)))))


; Cutting this bc it's similar to ghci, in that you lose track of the arguments.
; It's slightly better bc the args appear when forced, instead of jumping back to call site.
#;(slide
 (comment "")
 #:name "Watson"
 (colorize (t "Another semantics-based tool") "blue")
 (citation "Watson 1997")
 (blank)
 'next
 (vl-append
  (btt-sm "twice x = x + x")
  (blank gap-size)
  (btt-sm "twice (1 + 2)")
  (btt-sm "-> x + x")
  (btt-sm "-> (1 + 2) + x")
  (btt-sm "-> 3 + x")
  (btt-sm "-> 3 + 3")
  (btt-sm "-> 6"))
 )


(slide
 (comment 
  "Seeing this, it seems like the choice of semantics in a step-based lazy debugging tool matters.\n\n"
  "We would like our debugger to use a semantics that is \"intuitive\" and easy for programmers to understand.")
 #:name "Idea #3"
 (make-idea-title 3)
 (t "A more \"intuitive\" lazy semantics is needed."))


(define λneed|| @tex{\lambda_{need\parallel}})
(define λneed||-blue @tex{\textcolor{blue}{\lambda_{need\parallel}}})

(slide
 (comment
  "To summarize so far, we looked at feedback from lazy programmers and saw demand for a new, step-based lazy debugging tool.\n\n"
  "From reviewing previous work, we determined that it would be nice if our step-based tool was based on a semantics that is lazy, high-level, and intuitive and easy to understand.\n\n"
  "So that's what we did.")
 #:name "Our Work (title)"
 (colorize (t-lg "Our Work") "blue")
 'next
 (t "A step-based lazy debugging tool,")
 (t "based on a high-level \"intuitive\" lazy semantics."))

(slide
 (comment 
  "Our tool is an algebraic stepper for Lazy Racket, which is a lazy language that has the same thunk-based evaluation mechanism as Haskell.\n\n"
  "The steps in the debugger are based on a new high-level lazy semantics we developed, a rewriting system called λneed||, that we think is more intuitive for programmers to understand. And it will be evident soon why we chose this name.\n\n"
  "Theoretically, we show two things:\n\n"
  "1) That our λneed|| semantics corresponds to existing lazy semantics and is thus suitable for modeling lazy programs.\n\n"
  "2) Having a formal semantics for our tool allows us to prove our tool correct. This is especially important since we use a high-level semantics to model evaluation of lazy programs. Just like correct compilers are more reliable, we feel that it's important for tools like debuggers, which programmers also count on, to be correct as well.\n\n"
  )
 #:name "Our Work"
 #:title (colorize (t "Our Work") "blue")
 (item (ht-append (t "An algebraic stepper tool for Lazy Racket." 28)))
 'next
 (item (hc-append (t "A new, \"intuitive\" semantics for lazy languages, " 28) λneed||))
 'next
 (item (t "Theory:" 28))
 (subitem (hc-append λneed|| (t " corresponds to existing lazy semantics." 28)))
 'next
 (subitem (hc-append (t "Tool is correct with respect to " 28) λneed||))
  )



(slide
 (comment 
  "I keep using the word intuitive to describe our semantics. What exactly do I mean by this word? I mean two things.\n\n"
  "The first is that the semantics is syntactic. This means that we only do rewrites of the program source. A language's syntax is something that all programmers must learn and master so a syntactic semantics should be very easy for programmers to understand.\n\n"
  "The second thing I mean when I say that our semantics is intuitive is that it's based on substitution. Substitution is the fundamental notion computation on which all functional languages are based and it has been shown by many intro CS courses that substitution is very natural for all programmers to understand, including beginners, since it's just a generalization of high-school algebra.\n\n"
  "So that's why WE think the semantics is more intuitive. But to test our predictions, we introduced our tool to undergrads taking the intro PL course at Northeastern. Thus far preliminary feedback has confirmed our expectations.")
 #:name "λneed||: intuitive"
 #:title λneed||-blue
 (t "\"intuitive\"")
 'next
 (t "=")
 (t "syntactic")
 'next
 (t "+")
 (t "substitution-based"))

;; cutting this, I no longer think our semantics is graph reduction
;; it's BETTER because it doesnt have to do any copying (see example in ariola et al)
#;(slide
 (comment
  "For the laziness part, our semantics uses labels to express sharing.\n\n"
  "This makes it similar to graph reduction but as mentioned previously, we want to emphasize the source syntax only, so there's no conversion to combinators or any kind of graphic visualization.\n\n"
  "Thus, you can think of our semantics as a syntactic graph reduction.")
 #:name "λneed||: also"
 #:title (hc-append λneed||-blue (colorize (t ": Sharing") "blue"))
 (t "Uses labels to express sharing")
 'next
 (blank 10)
 (t "i.e., \"syntactic graph reduction\"")
 )

(slide
 (comment 
  "So hopefully by now, you're thinking, \"show me some examples!\". Well, here's some example programs.\n\n"
  "The first example is very basic. We have a definition of a function that doubles its argument, and then adds 5 to that. And then we have a call to the function with the argument 1+2. The first step deferences the function name and the second step applies the function. You can see that since the language is lazy, the argument is not evaluated and the delayed computation is substituted for all occurrences of the variable x. However, since all instances of the argument are actually shared, when one of the arguments is forced, like in the next step, all the instances are reduced in parallel. Thus lazy semantics is preserved because arguments only get reduced once. Note that the tool only shows you what is shared when one of the shared terms is being reduced. All other times, the program source is unadorned, making reductions simpler to look at.\n\n"
  "The next example is slightly more complicated and deals with an infinite list of natural numbers. Here we want to add the second element of the list to a computation that may or may not work. Since we are lazy, evaluation of nats is delayed in a thunk until it's needed by the call to second. The first step dereferences the nats thunk. Since second needs the second element of the list, we have to force the thunk to get more of the list, so the call to map is evaluated. In this step, we need the value of nats again but we're still in the middle of evaluating nats since there's a cycle, so we get this opaque thunk \"delayed evaluation 0\". The next step shows the result of the map, which returns some more thunks. Finally, we can evaluate second, which gives us one of the thunks. The addition requires this thunk to be forced. The ellipses on the left means that an opaque thunk is forced. And finally, we get the result 2. You can see that the original definition of nats has been updated as well, since it's shared.\n\n"
 "Trying to evaluate the other operand of the addition produces an error. At which time we can step backwards to check out the context of the error."
  )
 #:name "Demo"
 (colorize (t-lg "Demo!") "blue"))





(define (inE . strs)
  (if (number? (car strs))
      (string-append "\\textcolor{grayed}{E}_" (number->string (car strs)) 
                     "\\textcolor{grayed}{[}\\;\\;" (apply string-append (cdr strs)) "\\;\\;\\textcolor{grayed}{]}")
      (string-append "\\textcolor{grayed}{E[}\\;\\;" (apply string-append strs) "\\;\\;\\textcolor{grayed}{]}")))

(define (inwhiteE . strs)
  (if (number? (car strs))
      (string-append "\\textcolor{white}{E}_" (number->string (car strs)) 
                     "\\textcolor{white}{[}\\;\\;" (apply string-append (cdr strs)) "\\;\\;\\textcolor{grayed}{]}")
      (string-append "\\textcolor{white}{E[}\\;\\;" (apply string-append strs) "\\;\\;\\textcolor{white}{]}")))

(define (inblackE . strs)
  (if (number? (car strs))
      (string-append "\\textcolor{black}{E}_" (number->string (car strs)) 
                     "\\textcolor{black}{[}\\;\\;" (apply string-append (cdr strs)) "\\;\\;\\textcolor{black}{]}")
      (string-append "\\textcolor{black}{E[}\\;\\;" (apply string-append strs) "\\;\\;\\textcolor{black}{]}")))

(slide
 (comment
  "Now that you've seen some example programs in the stepper, I'm going to explain the semantics a bit more formally.\n\n")
 #:name "Semantics (title)"
 (colorize (t-lg "Semantics") "blue"))

(slide
 (comment 
  "Here's some syntax. Little e represents expressions. I've included only a minimal set of expressions for this presentation. The interesting part is the labeled expressions, at the end, which are used to indicate shared expressions.\n\n"
  "The big E are evaluation contexts, which are used to find the next redex.")
 #:title (hc-append λneed||-blue (colorize (t ": Syntax") "blue"))
 #:name "λneed||"
 (vl-append
;  @tex{\hspace{3pt}e = \lambda x.e \mid e\;e \mid \texttt{cons}\;e\;e \mid \ldots \mid e^\ell}
;  @tex{E = [\;] \mid E\;e \mid \ldots \mid E^\ell})
  @tex{\hspace{3pt}e = \lambda x.e \mid e\;e \mid \ldots \mid e^\ell}
  @tex{E = [\;] \mid E\;e \mid \ldots \mid E^\ell}
  @tex{\hspace{1cm}\ell \in \textrm{labels}})
 )

(define (flush-left p) (lc-superimpose p (blank (current-para-width) 0)))

(slide
 (comment 
  "A λneed|| rewriting step happens in two phases\n\n"
  "The first phase reduces a redex, as dictated by the evaluation contexts.\n\n"
  "Here is an example of a phase1 reduction, the reduction of an application, which you can see happens in an evaluation context.\n\n"
  "The label on the lambda with the vector represents any number of labels. Since our semantics is lazy, evaluation of arguments is delayed and a copy of the unevaluated argument is substituted for each variable instance in the function body. To avoid duplication of work when the arguments are forced, they are tagged with a unique label to indicate sharing.\n\n"
  "The second phase checks if the redex is under a label and if it is, all other identically labeled terms are similarly reduced.")
 #:name "λneed||: 2 phases"
 #:title (hc-append λneed||-blue (colorize (t ": Two-phase Steps") "blue"))
 'next
 (flush-left (t "1) Reduce next redex."))
 'next
 (ht-append
  @tex{@inE{(\lambda x.e_1)^{\vec{\ell}}\;e_2} \rightarrow\;}
  (vc-append -20
   @tex{@inE{e_1\{x:=e_2^{\ell_x}\}}}
   (hc-append @tex{\ell_x} (t-sm " fresh"))))
 (blank)
 'next
 (flush-left
  (htl-append 
   (t "2) ")
   (vl-append 
    (t "If redex is under a label, update all other")
    (t "identically labeled expressions to match."))))
 #;(bulleted-list-alts
  #:gap 40
  #:bullet 'numbered
  (t "Reduce next redex.")
  (vl-append 
   (t "If redex is under a label, update all other")
   (t "identically labeled expressions to match.")))
 )




#;(slide
 (comment 
  "Here is an example of a phase1 reduction, the reduction of an application, which happens in an evaluation context.\n\n"
  "The label on the lambda with the vector represents any number of labels. Since our semantics is lazy, evaluation of arguments is delayed and a copy of the unevaluated argument is substituted for each variable instance in the function body. To avoid duplication of work when the arguments are forced, they are tagged with a unique label to indicate sharing and to ensure that they all reduce in parallel.\n\n"
  )
 #:title (hc-append λneed||-blue (colorize (t ": Phase 1") "blue"))
 #:name "λneed||: Phase 1"
 (ht-append
  @tex{@inE{(\lambda x.e_1)^{\vec{\ell}}\;e_2} \rightarrow\;}
  (vc-append -20
   @tex{@inE{e_1\{x:=e_2^{\ell_x}\}}}
   (hc-append @tex{\ell_x} (t-sm " fresh")))))

#;(slide
 (comment 
  "And as mentioned previously, evaluation contexts dictate which redex to reduce next.")
 #:title (hc-append λneed|| (t ": phase 1"))
 #:name "λneed||: phase 1"
 (ht-append
  @tex{@inE{(\lambda x.e_1)^{\vec{\ell}}\;e_2} \rightarrow\;}
  (vc-append -20
   @tex{@inE{e_1\{x:=e_2^{\ell_x}\}}}
   (hc-append @tex{\ell_x} (t-sm " fresh")))))


#;(slide
 (comment 
  "Here are some other reduction rules. Constructors are lazy, so if either of the arguments are not labeled, then they get fresh labels. Having multiple labels on an expression is handled correctly by the system.\n\n"
  "The accessor reductions work as expected: if you have a cons cell, which may be under any number of labels, then extract the appropriate component.")
 #:title (hc-append λneed|| (t ": phase 1"))
 #:name "λneed||: phase 1"
 (ht-append
  (vc-append -20
   @tex{@inE{\texttt{cons}\;e_1\;e_2}}
   (hb-append @tex{e_1} (t-smm " unlabeled  or  ") @tex{e_2} (t-smm " unlabeled")))
  (vc-append -20
   @tex{\rightarrow\; @inE{\texttt{cons}\;e_1^{\ell_1}\;e_2^{\ell_2}}}
   (hc-append @tex{\ell_1,\ell_2} (t-smm " fresh"))))
 (vr-append
  @tex{@inE{\texttt{first}\;(\texttt{cons}\;e_1\;e_2)^{\vec{\ell}}} \rightarrow @inE{e_1}}
  @tex{@inE{\texttt{rest}\;(\texttt{cons}\;e_1\;e_2)^{\vec{\ell}}} \rightarrow @inE{e_2}}))

(define ellip1 (linewidth 3 (gray (ellipse 35 60) 3)))
(define ellip2 (linewidth 3 (gray (ellipse 35 60) 3)))
(define ellip3 (colorize (ellipse 35 60) "white"))

;(define slide-λneed||-phase2-contents-1
;  (vc-append -20
;   @tex{@inblackE{e} \rightarrow @inblackE{e'}}
;   (t-sm "(phase 1)")))
;(define slide-λneed||-phase2-contents-2
;  (vc-append -20
;   @tex{E = @inblackE[1]{(@inblackE[2]{})^\ell}}
;   (hc-append (t-sm "no labels in ") @tex{E_2})))
;(define slide-λneed||-phase2-1
; (vc-append gap-size
;  slide-λneed||-phase2-contents-1
;  (ghost (it "and"))
;  (ghost slide-λneed||-phase2-contents-2)))
;(define slide-λneed||-phase2-2
; (vc-append gap-size
;  (lt-superimpose
;   slide-λneed||-phase2-contents-1
;   (hc-append ellip1 (blank 150 0) ellip2))
;  (ghost (it "and"))
;  (ghost slide-λneed||-phase2-contents-2)))
;(define slide-λneed||-phase2-3
;  (pin-arrow-line 16
;   (pin-arrow-line 16
;    (vc-append gap-size
;     (lt-superimpose
;      slide-λneed||-phase2-contents-1
;      (hc-append ellip1 (blank 150 0) ellip2))
;     (it "and")
;     (lt-superimpose
;      slide-λneed||-phase2-contents-2
;      ellip3))
;    ellip1 cb-find ellip3 ct-find
;    #:line-width 3 #:color (scale-color 3 "black"))
;   ellip2 cb-find ellip3 rt-find
;   #:line-width 3 #:color (scale-color 3 "black")))

#;(slide
 (comment 
  "If a redex e reduces to e' in phase 1, and in the surrounding context, the redex is under a label, say ell, then in the second phase, reduce all other similarly labeled expressions. This is how all copies of an argument get reduced in parallel.")
 #:name "λneed||: Phase 2"
 #:title (hc-append λneed|| (t ": phase 2"))
 (it "if")
 'alts
 (list
  (list slide-λneed||-phase2-1)
  (list slide-λneed||-phase2-2)
  (list slide-λneed||-phase2-3))
 #;(pin-arrow-line 16
 (vc-append gap-size
  (lt-superimpose
   (vc-append -20
    @tex{@inblackE{e} \rightarrow @inblackE{e'}}
    (t-sm "(phase 1)"))
   (hc-append ellip1 (blank 150 0) ellip2))
  (it "and")
  (vc-append -20
   (lt-superimpose
    @tex{E = @inblackE[1]{(@inblackE[2]{})^\ell}}
    ellip3)
   (hc-append (t-sm "no labels in ") @tex{E_2})))
 ellip1 cb-find ellip3 ct-find
 #:line-width 3 #:color (scale-color 2 "black"))
 'next
 (blank)
 (it "then")
 (hc-append (t " update all ") @tex{\ell} {t "-labeled terms in "} @tex{E_1})
 )

(slide
 (comment
  "Here is a diagram illustrating the two phases of a step.\n\n"
  "This is a tree representation of a program, where the redex little e is in evaluation context big E.\n\n"
  "In phase 1, little e is reduced to little e'.\n\n"
  "Then, if the redex is under a label, say ell, then in phase 2, all other instances of ell-labeled expressions are reduced in parallel."
  )
 #:name "λneed||: Two-phase Steps, Pictorially"
 #:title (hc-append λneed||-blue (colorize (t ": Two-phase Steps, Pictorially") "blue"))
 'alts
 (list
  (list (bitmap "ifl2011-slide-phase2-1.png"))
  (list (bitmap "ifl2011-slide-phase2-2.png"))
  (list (bitmap "ifl2011-slide-phase2-3.png"))
  (list (bitmap "ifl2011-slide-phase2-4.png"))
  (list (bitmap "ifl2011-slide-phase2-5.png")))
 )




#;(slide
 (comment 
  "Our tool is implemented in Racket, specifically Lazy Racket, which is a lazy language with the same basic evaluation mechanism as Haskell.")
 #:name "Why Racket?"
 (t-lg "Why Racket?"))

(slide
 (comment
  "Now I'm going to talk about how we implemented our stepper")
 #:name "Implementation (title)"
 (colorize (t-lg "Implementation") "blue"))

(slide
 (comment 
  "We implemented our tool in Racket to take advantage of continuation marks, which is a mechanism built into Racket that provides an easy way to get access to the stack. They were introduced by Clements in 2001.\n\n"
  "There's only two operations involved. with-continuation-mark takes a tag and an expression e and marks the current stack frame with the tag while returning the result of e. The current-continuation-mark operation retrieves all the marks on the stack.\n\n"
  "Despite their simplicity, many features of Racket are built with continuation marks: ...\n\n"
  "All of these tools and constructs need access to the stack and continuation marks allow them to be built without modifying any of the underlying machinery of the language."
  )
 #:name "Continuation Marks"
 (colorize (t "Continuation Marks") "blue")
 (hc-append (t-sm "Mechanism for lightweight stack access. ")
            (citation "Clements 2001"))
 (scale (bitmap "ifl2011-slide-cont-mark.png") 0.9)
 'next
 (t-sm "Continuation marks used in Racket implementation of:")
 'next
 (it-sm "stack tracer, stepper, debugger, profiler, exception handling,")
 (it-sm "dynamic binding, delimited continuations, web server")
 #;(t-smm "also: security via stack inspection, aop")
 )



(slide
 (comment 
  "Here is the architecture of our lazy stepper. There are three steps to the operation of the stepper, represented by ovals in this diagram.\n\n"
  "In the first step, following several previous debugging tools, we use an instrumentation strategy, and so we annotate our lazy progam with continuation mark operations.\n\n"
  "In the second step, the annotated program is run, outputting continuation marks that represent stack traces.\n\n"
  "Finally, the stack traces are then used to reconstruct the desired lazy rewriting steps.\n\n")
 #:name "Stepper Architecture"
 #:title (colorize (t "Stepper Architecture") "blue")
 (scale (bitmap "ifl2011-slide-stepper-arch.png") 1)
 )

(slide
 (comment 
  "So the next question one might ask is, 'Is this just a Racket-specific tool? You mentioned a lot of Haskell in the beginning but now you are presenting a tool for Lazy Racket.' The answer to this question is no, this is not just a Racket-specific tool, and the reason is continuation marks. Continuation marks are very lightweight and are easily added to any language.\n\n"
  "In fact, Clements et al. showed that continuation marks can be added to Javascript.\n\n"
  "Additionally, Allwood et al. independently came up with a construct that is essentially the same as continuation marks in developing a tool for Haskell. They found that continuation marks were useful for generating stack traces in GHC.\n\n"
  "So since continuation marks are easily implementable, our tool is also easily portable to any lazy language.")
 #:name "Continuation Marks are Portable"
 (colorize (t "Continuation marks are easily added to any language.") "blue")
 'next
 (citation "\"Implementing continuation marks in JavaScript\" (Clements et al., 2008)")
 'next
 (citation "\"Finding the needle: stack traces for GHC\" (Allwood et al., 2009)"))





 
(slide
 (comment 
  "On the theory side, we show that there's a correspondence between our semantics and existing lazy semantics. Specifically, we show a correspondence between both a low-level semantics like Launchbury, and a reduction semantics like Ariola et al\n\n"
  "Thus, our semantics is appropriate for modeling lazy programs.")
 #:name "λneed|| Correctness"
 #:title (hc-append λneed||-blue (colorize (t ": Correctness") "blue"))
 (hc-append (t "Correspondence exists between ") λneed|| (t " and:"))
 'alts
 (bulleted-list-alts
  #:gap 20
  #:bullet "-"
  (t-sm "Low-level semantics (i.e., Launchbury)")
  (t-sm "Reduction semantics (i.e., Ariola et al.)")))

(slide
 (comment
  "Our tool has proven useful in a classroom setting so far but there are some improvements that we are investigating, to make our tool more practical.\n\n"
  "First, additional navigation features are needed, like breakpointing, so programmers dont have to step through a program from the beginning every time.\n\n"
  "Second, it would be nice if the tool allowed more detailed inspection of the program state at each step, such as being able to look at variable bindings.\n\n"
  "And third, as programs get larger, subsitution can become unwieldy, so we are currently looking into ways of addressing the scaling issue.")
 #:name "To Do"
 (colorize (t "To Do") "blue")
 'next
 (item "Advanced navigation features, breakpointing")
 'next
 (item "Additional inspection of program state")
 'next
 (item "Scaling to large programs"))

(slide
 (comment
  "In summary, we saw there was demand for a new step-based lazy debugging tool so we set out to make one. We wanted our tool to use an easy to understand lazy semantics, but there didnt seem to exist one we liked, so we came up with a new semantics for lazy evaluation, λneed||, which we think is suitable clarifying the confusing nature of laziness. And we also show that our semantics is equivalent to existing lazy semantics.\n\n"
  "We implemented a stepper tool for Lazy Racket based on this semantics, which allowed us to prove the tool correct. Also, we used a portable implementation technique, so our tool can easily be implemented in any lazy language.\n\n"
  "Thanks!")
 #:title (colorize (t "Summary") "blue")
 #:name "Summary"
 (item (hc-append (t "New semantics for lazy evaluation: ") λneed||))
 (subitem (t-sm "Easy to understand and suitable for use in a debugger."))
 (subitem (t-sm "Equivalent to existing lazy semantics."))
 (item (hc-append (t "Algebraic stepper for Lazy Racket, based on ") λneed||))
 (subitem (t-sm "Proven correct."))
 (subitem (t-sm "Easily ported to any lazy language via continuation marks."))
 'next
 (blank)
 (t "Thanks!")
 (btt-sm "http://racket-lang.org/")
 )

#;(re-slide (most-recent-slide) (rb-superimpose full-page (rectangle 50 50)))