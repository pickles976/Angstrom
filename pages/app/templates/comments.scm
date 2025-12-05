#!/usr/bin/env -S csi -s

;;; comments.scm - Display comments from database using ORM

(import (chicken string))
(import srfi-1)
(import srfi-13)
(import micron)
(import orm)

;; Note: This file expects settings.scm to be loaded by the calling script
;; (which provides app-models-path parameter)

;; Initialize ORM with models from configured path
(orm-init (app-models-path))

;; ========== COMMENTS DISPLAY ==========

(define (render-comment comment)
  "Render a single comment instance as micron markup"
  (let ((name (alist-ref 'name comment))
        (text (alist-ref 'text comment))
        (timestamp (alist-ref 'timestamp comment))
        (address (alist-ref 'address comment)))
    (conc
      (style '(fg "eee")) nl name (reset-style)
      " (" timestamp ")"
      (if (and address (not (string-null? address)))
          (conc
            (style '(fg "0FD"))
            (link (conc "lxmf@" address) (conc " lxmf@" address))
            (reset-style))
          "")
      ":" nl
      text nl "-" nl)))

(define (display-comments db-path page-id)
  "Display comments for a page from the database"
  ;; Open database connection
  (db-open db-path)

  ;; Query comments for this page
  (define comments (db-list 'comment `((page-name . ,page-id))))

  ;; Close database
  (db-close)

  ;; Render comments or show message
  (if (null? comments)
      "No comments yet. Be the first!"
      ;; Reverse to show newest first
      (apply conc (map render-comment (reverse comments)))))
