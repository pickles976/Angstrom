#!/usr/bin/env -S csi -s

(load "./framework/micron-dsl.scm")
(load "./framework/pages/app/readers.scm")

(define comments-dir "/home/sebas/Projects/NomadnetTemplates/framework/pages/comments")
(define page-name "index")

(define (my-input-field label fieldname size)
  (conc (style '(bg "333" fg "aaa")) (input-field-fixed fieldname size) (reset-style) label newline))

;; Generate the page
(print

  squiggle newline
  (center 
    (bold "Hello! ") 
    "This is output from " 
    (italics "micron")) 
  newline squiggle newline
    
  (section "Main")
    (style '(align left))
"This is a multi-line string. Does Chicken scheme have support for those?
Huh, I guess it does."
    newline
    (style '(fg "33f"))
    (link "lxmf@exampleaddress" "Link Example")
    (style '(fg "aaa"))
    newline

    (style '(align center)) newline
    
    (subsection "Comments")
      (display-comments comments-dir page-name)
      
    (subsection "Leave a Comment")
      (style '(fg "aaa" align left))
      (my-input-field  " Name " "user_name" 16)
      (my-input-field  " LXMF Address (optional)" "user_lxmf" 32)
      (my-input-field  " Comment " "comment_text" 64)
      
      (style '(bg "373"))
      ;; label, link, page name, fields
      (submit-field "Submit" "/app/handle_comment.mu" "index" "user_name" "user_lxmf" "comment_text")
      (reset-style))