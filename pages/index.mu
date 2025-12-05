#!/usr/bin/env -S csi -s

;;; index.mu - Angstrom Framework Homepage

(import micron)
(import markdown)
(import (chicken process-context))
(import (chicken file))
(import (chicken port))

;; ========== DEBUG INFO ==========
;; Print debug information to stderr
; (with-output-to-port (current-error-port)
;   (lambda ()
;     (print "=== DEBUG INFO ===")
;     (print "Working Directory: " (current-directory))
;     (print "")
;     (print "Environment Variables:")
;     (for-each
;       (lambda (env-pair)
;         (print "  " (car env-pair) " = " (cdr env-pair)))
;       (get-environment-variables))
;     (print "==================")
;     (print "")))

;; Load settings (running from pages/, so app/ is relative)
(load "/home/sebas/.nomadnetwork/storage/pages/app/settings.scm")
(load "/home/sebas/.nomadnetwork/storage/pages/app/templates/header.scm")
(load "./app/templates/recent-comments.scm")

;; Configuration
(define page-name "index")

(define (my-input-field label fieldname size)
  (conc (style '(bg "333" fg "aaa")) (input-field-fixed fieldname size) (reset-style) label nl))

;; Generate the page
(print

  (Angstrom-header)

  ;; Welcome section
  (style '(align left))
  nl

  nl
  (section "Welcome to Angstrom")
  nl

    (style '(fg "ddd"))
    "This is a framework written (mostly by claude) to allow for the easy"
    nl
    "construction of " (bold "interactive pages") " on nomadnet."
    nl nl

    (style '(fg "5af"))
    "https://github.com/pickles976/Angstrom"
    (style '(fg "ddd"))
    nl nl

    "I found that there was not enough easy to use tooling around nomadnet"
    nl
    "and micron, and I hope that Angstrom helps bridge that gap."
    nl nl

    (style '(fg "5af"))
    (file-link "/file/Angstrom.tar.gz" "Download Angstrom.tar.gz")
    (style '(fg "ddd"))
    nl

  ;; Installation instructions from markdown
  (style '(align left))
  nl
  (md-file->micron "./app/markdown/index.md")
  nl

  nl
  (section "Reference")
  nl
  "Here is some documentation to get you started:"
  nl

  nl
  (style '(fg "5af"))
  (bold (file-link "/page/subpages/chicken-scheme-basics.mu" "Learn the fundamentals of Scheme programming "))
  nl nl
  (bold (file-link "/page/subpages/micron-dsl.mu" "Generate micron with scheme "))
  nl nl
  (bold (file-link "/page/subpages/markdown-converter.mu" "Write content efficiently with markdown "))
  nl nl
  (bold (file-link "/page/subpages/orm.mu" "Build data-driven applications with our simple ORM "))
  (style '(fg "ddd"))
  nl

  ;; Recent Comments section (uses raw SQL - see template for learning example)
  nl
  (section "Recent Comments (All Pages)")
  nl

    (style '(align left fg "ddd"))
    nl
    (display-recent-comments (app-db-path) 10)
    nl

  nl
  (subsection "Leave a Comment")
  nl

    (style '(fg "aaa" align left))
    nl
    (my-input-field  " Name " "user_name" 16) nl
    (my-input-field  " LXMF Address (optional)" "user_lxmf" 32) nl
    (my-input-field  " Comment " "comment_text" 64) nl

    (style '(bg "373"))
    (submit-field "Submit" "/app/actions/handle_comment.scm" page-name "user_name" "user_lxmf" "comment_text")
    (reset-style))
