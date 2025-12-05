#!/usr/bin/env -S csi -s

;;; index.mu - Macron Framework Homepage

(import micron)
(import markdown)

;; Load settings (running from pages/, so app/ is relative)
(load "app/settings.scm")
(load "app/templates/header.scm")
(load "app/templates/comments.scm")

;; Configuration
(define page-name "index")

(define (my-input-field label fieldname size)
  (conc (style '(bg "333" fg "aaa")) (input-field-fixed fieldname size) (reset-style) label nl))

;; Generate the page
(print

  (macron-header)

  ;; Welcome section
  (style '(align left))
  nl

  nl
  (section "Welcome to Macron")
  nl

    (style '(fg "ddd"))
    "This is a framework written (mostly by claude) to allow for the easy"
    nl
    "construction of " (bold "interactive pages") " on nomadnet."
    nl nl

    (style '(fg "5af"))
    (link "https://github.com/pickles976/Macro" "View on GitHub")
    (style '(fg "ddd"))
    nl nl

    "I found that there was not enough easy to use tooling around nomadnet"
    nl
    "and micron, and I hope that macron helps bridge that gap."
    nl nl

    (style '(fg "5af"))
    (link "/file/macron.tar.gz" "Download macron.tar.gz")
    (style '(fg "ddd"))
    nl

  ;; Docs section
  nl
  (subsection "Documentation")
  nl

    (style '(fg "ddd" align left))
    nl
    "The " (code "docs") " folder has scheme files that explain how things work."
    nl

  ;; Installation instructions from markdown
  (style '(align left))
  nl
  (md-file->micron "app/markdown/index.md")
  nl

  ;; Comments section
  nl
  (section "Community Discussion")
  nl

    (style '(align left fg "ddd"))
    nl
    (display-comments (app-db-path) page-name)
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
    ;; label, link, page name, fields
    (submit-field "Submit" "/app/actions/handle_comment.scm" page-name "user_name" "user_lxmf" "comment_text")
    (reset-style))
