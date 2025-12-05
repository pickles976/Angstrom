#!/usr/bin/env -S csi -s

;;; index.mu - Macron Framework Homepage

;; Paths relative to workspace root (where this is run from)
(load "framework/micron.scm")
(load "framework/markdown.scm")
(load "app/templates/comments.scm")

;; Configuration
(define db-path "app/app.db")
(define page-name "index")

(define (my-input-field label fieldname size)
  (conc (style '(bg "333" fg "aaa")) (input-field-fixed fieldname size) (reset-style) label newline))

;; Generate the page
(print

  squiggle newline
  (center
    (bold (style '(fg "3af")) "MACRON" (reset-style))
    newline
    (italics "Interactive Micron Apps Made Easy"))
  newline squiggle newline

  ;; Main content from markdown
  (style '(align left))
  (md-file->micron "app/markdown/index.md")
  newline

  ;; Comments section
  (divider) newline
  (section "Community Discussion")
    (style '(align left))
    (display-comments db-path page-name)
    newline

  (subsection "Leave a Comment")
    (style '(fg "aaa" align left))
    (my-input-field  " Name " "user_name" 16) newline
    (my-input-field  " LXMF Address (optional)" "user_lxmf" 32) newline
    (my-input-field  " Comment " "comment_text" 64) newline

    (style '(bg "373"))
    ;; label, link, page name, fields
    (submit-field "Submit" "/app/actions/handle_comment.scm" page-name "user_name" "user_lxmf" "comment_text")
    (reset-style))
