#!/usr/bin/env -S csi -s

;;; comments.scm - Display comments using both ORM and raw SQL

(import (chicken string))
(import srfi-1)
(import srfi-13)
(import sql-de-lite)
(import micron)
(import orm)

;; Note: This file expects settings.scm to be loaded by the calling script
;; (which provides app-models-path parameter)
(load "/home/sebas/Projects/NomadnetTemplates/pages/app/settings.scm")

;; Initialize ORM with models from configured path
(orm-init (app-models-path))

;; ========== SHARED COMMENT RENDERING ==========

(define (render-comment name address timestamp text #!optional page-name)
  "Render a single comment with consistent formatting.
   If page-name is provided, it will be displayed."
  (conc
    ;; Name and timestamp with arrow
    (style '(fg "aa2")) "▶ " name (reset-style)
    " (" timestamp ")"

    ;; Optional LXMF address
    (if (and address (not (string-null? address)))
        (conc
          (style '(fg "0FD"))
          (link (conc "lxmf@" address) (conc " lxmf@" address))
          (reset-style))
        "")

    ;; Show which page this comment is on (if provided)
    (if page-name
        (conc " on " (style '(fg "5af")) page-name (reset-style))
        "")
    ":" nl

    ;; Comment text with background and indentation
    nl
    (style '(fg "eee" bg "222")) "    " text (reset-style)
    nl
    nl))

;; ========== ORM-BASED COMMENTS (FOR SPECIFIC PAGE) ==========

(define (display-comments db-path page-id)
  "Display comments for a specific page using ORM"
  ;; Open database connection
  (db-open db-path)

  ;; Query comments for this page
  (define comments (db-list 'comment `((page-name . ,page-id))))

  ;; Close database
  (db-close)

  ;; Render comments or show message
  (if (null? comments)
      (conc (style '(fg "888")) "No comments yet. Be the first!" (reset-style) nl)

      ;; Reverse to show newest first, then render each
      (apply conc
        (map (lambda (comment)
               (render-comment
                 (alist-ref 'name comment)
                 (alist-ref 'address comment)
                 (alist-ref 'timestamp comment)
                 (alist-ref 'text comment)))
             (reverse comments)))))

;; ========== RAW SQL COMMENTS (ACROSS ALL PAGES) ==========

(define (display-recent-comments db-path limit)
  "Display the most recent N comments from all pages using raw SQL"

  ;; Open database and execute query
  (let* ((db (open-database db-path))
         (sql-query "SELECT id, name, address, page_name, timestamp, text
                     FROM comment
                     ORDER BY id DESC
                     LIMIT ?")
         (rows (query fetch-all (sql db sql-query) limit)))

    ;; Close database
    (close-database db)

    ;; Process results
    (if (null? rows)
        (conc (style '(fg "888")) "No comments yet." (reset-style) nl)

        ;; Render each comment, including page-name since we're showing all pages
        (apply conc
          (map (lambda (row)
                 (let ((id         (list-ref row 0))
                       (name       (list-ref row 1))
                       (address    (list-ref row 2))
                       (page-name  (list-ref row 3))
                       (timestamp  (list-ref row 4))
                       (text       (list-ref row 5)))

                   (render-comment name address timestamp text page-name)))
               rows)))))

;; =========== Custom User Comment Field ===================


(define (my-input-field label fieldname size)
  (conc (style '(bg "333" fg "aaa")) (input-field-fixed fieldname size) (reset-style) label nl))


(define (comment-section page-id)

  (conc
    nl
    (section "Comments")
    nl
  
      (style '(align left fg "ddd"))
      (display-comments (app-db-path) page-name)

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
        (reset-style)))
