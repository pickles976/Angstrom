#!/usr/bin/env -S csi -s

;;; recent-comments.scm - Display recent comments using RAW SQL
;;;
;;; This template demonstrates how to bypass the ORM and use sql-de-lite directly.
;;; Use this approach when you need:
;;;   - Complex queries (JOINs, aggregations, etc.)
;;;   - Performance optimization
;;;   - SQL features not supported by the ORM

(import (chicken string))
(import srfi-1)
(import srfi-13)
(import sql-de-lite)  ; Direct SQLite access
(import micron)

;; ========== RAW SQL EXAMPLE ==========

(define (display-recent-comments db-path limit)
  "Display the most recent N comments from the database using raw SQL"

  ;; Step 1: Open the database connection
  ;; Unlike the ORM's db-open, we use sql-de-lite's open-database directly
  (let* ((db (open-database db-path))

         ;; Step 2: Write your SQL query
         ;; This query gets the last N comments, ordered by ID (newest first)
         ;; Note: We're querying ALL pages, not filtering by page-name
         (sql-query "SELECT id, name, address, page_name, timestamp, text
                     FROM comment
                     ORDER BY id DESC
                     LIMIT ?")

         ;; Step 3: Prepare the query statement
         (stmt (sql db sql-query))

         ;; Step 4: Execute the query with parameters
         ;; The ? in the query is replaced with our limit value
         ;; query fetch-all returns a list of rows (each row is a list of values)
         (rows (query fetch-all (sql db sql-query) limit)))

    ;; Step 5: Close the database when done
    (close-database db)

    ;; Step 6: Process the results
    ;; Each row is a list in this order: (id name address page-name timestamp text)
    (if (null? rows)
        ;; No comments found
        (conc (style '(fg "888")) "No comments yet." (reset-style) nl)

        ;; Render each comment
        ;; We use let-values or list-ref to extract fields from each row
        (apply conc
          (map (lambda (row)
                 (let ((id         (list-ref row 0))
                       (name       (list-ref row 1))
                       (address    (list-ref row 2))
                       (page-name  (list-ref row 3))
                       (timestamp  (list-ref row 4))
                       (text       (list-ref row 5)))

                   ;; Render the comment with micron markup
                   (conc
                     ;; Name and timestamp
                     (style '(fg "eee")) name (reset-style)
                     " (" timestamp ")"

                     ;; Optional LXMF address
                     (if (and address (not (string-null? address)))
                         (conc
                           (style '(fg "0FD"))
                           (link (conc "lxmf@" address) (conc " lxmf@" address))
                           (reset-style))
                         "")

                     ;; Show which page this comment is on
                     " on " (style '(fg "5af")) page-name (reset-style)
                     ":" nl

                     ;; Comment text
                     (style '(fg "ddd")) text (reset-style) nl
                     "-" nl)))

               rows)))))
