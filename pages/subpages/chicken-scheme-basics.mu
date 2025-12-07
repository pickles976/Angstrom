#!/usr/bin/env -S csi -s

;;; chicken-scheme-basics.mu - Introduction to Chicken Scheme

(import micron)
(load "/home/sebas/Projects/NomadnetTemplates/pages/app/settings.scm")
(load "/home/sebas/Projects/NomadnetTemplates/pages/app/templates/comments.scm")
(load "/home/sebas/Projects/NomadnetTemplates/pages/app/templates/code.scm")
(define page-name "chicken-scheme-basics")

(print
  (style '(align center))
  (section "Chicken Scheme Basics")
  (style '(align left))
  nl nl

  (subsection "What is Scheme?")
  nl
  (style '(fg "ddd"))
  "Scheme is a minimalist Lisp dialect that emphasizes simplicity and elegance."
  nl
  "Chicken Scheme compiles to C, making it fast and portable."
  nl nl

  (subsection "Basic Syntax")
  nl
  (style '(fg "5af"))
  "Everything is an expression in parentheses:"
  (reset-style) nl nl

  (code-block
  (code "(+ 1 2)") "  ; Returns 3" nl
  (code "(string-append \"Hello\" \" \" \"World\")") "  ; Returns \"Hello World\"" nl
  (code "(if (> 5 3) \"yes\" \"no\")") "  ; Returns \"yes\"" nl nl)

  (subsection "Defining Variables")
  nl
  (code-block
  (code "(define name \"Alice\")") nl
  (code "(define age 30)") nl
  (code "(define greeting (string-append \"Hello, \" name))") nl nl)

  (subsection "Functions")
  nl
  "Define functions with " (code "define") ":" nl nl
  (code-block
  (code "(define (greet name)") nl
  (code "  (conc \"Hello, \" name \"!\"))") nl nl
  (code "(greet \"Bob\")") "  ; Returns \"Hello, Bob!\"" nl nl)

  (subsection "Lists")
  nl
  "Lists are fundamental in Scheme:" nl nl
  (code-block
  (code "(list 1 2 3)") "  ; Create a list" nl
  (code "(car '(1 2 3))") "  ; Get first element (1)" nl
  (code "(cdr '(1 2 3))") "  ; Get rest of list (2 3)" nl
  (code "(cons 0 '(1 2 3))") "  ; Add to front (0 1 2 3)" nl nl)

  (subsection "Association Lists (Alists)")
  nl
  "Key-value pairs used throughout Angstrom:" nl nl
  (code-block
  (code "(define person '((name . \"Alice\") (age . 30)))") nl
  (code "(alist-ref 'name person)") "  ; Returns \"Alice\"" nl nl)

  (subsection "Common Functions")
  nl
  (bold "Map:") " Apply function to each element" nl
  (code-block
  (code "(map (lambda (x) (* x 2)) '(1 2 3))") "  ; Returns (2 4 6)" nl nl)

  (bold "Filter:") " Keep elements matching predicate" nl
  (code-block
  (code "(filter odd? '(1 2 3 4))") "  ; Returns (1 3)" nl nl)

  (bold "For-each:") " Side effects on each element" nl
  (code-block
  (code "(for-each print '(\"a\" \"b\" \"c\"))") "  ; Prints a, b, c" nl nl)

  (subsection "Let Bindings")
  nl
  "Create local variables:" nl nl
  (code-block
  (code "(let ((x 10)") nl
  (code "      (y 20))") nl
  (code "  (+ x y))") "  ; Returns 30" nl nl)

  (subsection "Learn More")
  nl
  (style '(fg "5af"))
  (link "https://www.call-cc.org/manual/" "Chicken Scheme Manual")
  (reset-style) nl
  (style '(fg "5af"))
  (file-link "/page/index.mu" "Back to Home")
  (reset-style)
  nl
  (comment-section page-name))
