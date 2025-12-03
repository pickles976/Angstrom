#!/usr/bin/csi -script

(import srfi-19)
(import (chicken process-context))
(import (chicken string))

(define user-name (or (get-environment-variable "field_user_name") "Anonymous"))
(define user-lxmf (or (get-environment-variable "field_user_lxmf") ""))
(define comment-text (or (get-environment-variable "field_comment_text") ""))
(define post-id (or (get-environment-variable "var_post_id") "default"))

;; Sanitize input (basic example)
(define sanitized-name (string-translate user-name #\` #\'))
(define sanitized-text (string-translate comment-text #\` #\'))

;; Generate timestamp
(define timestamp (date->string (current-date) "~Y-~m-~d ~H:~M"))

;; Build the file path
(define file-path (string-append "/home/sebas/.nomadnetwork/storage/pages/comments/"
                                 post-id ".txt"))

;; Append to comments file
(with-output-to-file file-path
  (lambda ()
    (display (string-append sanitized-name "|"
                           user-lxmf "|"
                           timestamp "|"
                           sanitized-text "\n")))
  #:append)

;; Redirect back to blog page with success message
(display "`c`!Thank you for your comment!`!`a\n")
(display "-\n")
(display "Your comment has been submitted successfully.\n")
(display "`F00a`_`[Return to blog`:/page/index.mu]`_`f\n")