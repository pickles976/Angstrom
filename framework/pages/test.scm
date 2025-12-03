#!/usr/bin/env -S csi -s
;;; blog-page.scm - Generates the blog page using micron-dsl

(load "./framework/micron-dsl.scm")

;; Generate the page
(print (render
  (micron
    squiggle newline
    (center 
      (bold "Hello! ") 
      "This is output from " 
      (italics "micron")) newline
    squiggle newline
    
    (section "Main"

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
      
      (subsection "Leave a Comment"
        (style '(fg "aaa" align left))
        (field " Name " "user_name" 16)
        (field " LXMF Address (optional)" "user_lxmf" 32)
        (field " Comment " "comment_text" 64)
        
        (style '(bg "373"))
          ;; label, link, page name, fields
          (submit "Submit" "/app/handle_comment.mu" "index" 
            "user_name" "user_lxmf" "comment_text"))
        "`b"))))