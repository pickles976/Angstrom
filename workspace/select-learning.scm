#!/usr/bin/env -S csi -s

;;; select-learning.scm - Learning SELECT queries
;;;
;;; We want to build a db-list function that:
;;; 1. Takes a model name and optional filters
;;; 2. Generates SELECT SQL with WHERE clause
;;; 3. Returns instances (alists) from database rows

(import scheme)
(import (chicken base))
(import (chicken string))
(import srfi-1)
(import srfi-13)
(import sql-de-lite)

;; ========== EXAMPLE 1: Basic SELECT ==========

(print "=== Example 1: Basic SELECT ===")

;; SQL we want to generate:
;; SELECT * FROM comment

(define db (open-database "app.db"))

;; Simple query - no WHERE clause
(define stmt (sql db "SELECT * FROM comment"))
(define rows (query fetch-all stmt))

(print "All comments:")
(print "Number of rows: " (length rows))
(print "First row: " (car rows))
(newline)

;; ========== EXAMPLE 2: SELECT with WHERE ==========

(print "=== Example 2: SELECT with WHERE ===")

;; SQL we want to generate:
;; SELECT * FROM comment WHERE page_name = ?

(define stmt2 (sql db "SELECT * FROM comment WHERE page_name = ?"))
(define filtered-rows (query fetch-all stmt2 "blog_post"))

(print "Comments for blog_post:")
(print "Number of rows: " (length filtered-rows))
(if (not (null? filtered-rows))
    (print "First row: " (car filtered-rows))
    (print "No rows found"))
(newline)

;; ========== EXAMPLE 3: Understanding Row Format ==========

(print "=== Example 3: Understanding Row Format ===")

;; sql-de-lite returns rows as lists of values
;; For comment table: (id name address page_name timestamp text)

(define example-row (car rows))
(print "Row as list: " example-row)
(print "Row is a list of " (length example-row) " values")

;; We need field names to convert to alist
(define comment-fields '(id name address page_name timestamp text))

;; Zip fields and values together
(define (row->alist fields values)
  "Convert a database row (list of values) to an alist"
  (map cons fields values))

(define comment-alist (row->alist comment-fields example-row))
(print "Row as alist: " comment-alist)
(newline)

;; ========== EXAMPLE 4: Building WHERE Clause ==========

(print "=== Example 4: Building WHERE Clause ===")

;; Input: filters as alist
(define filters '((page-name . "blog_post") (name . "Alice")))

;; Need to generate: "page_name = ? AND name = ?"
;; And extract values: ("blog_post" "Alice")

(define (filter->where-clause filter)
  "Convert a single filter (field . value) to SQL"
  (let ((field-name (symbol->string (car filter))))
    ;; Replace hyphens with underscores
    (string-translate field-name #\- #\_)))

(define where-parts
  (map (lambda (filter)
         (conc (filter->where-clause filter) " = ?"))
       filters))

(define where-clause (string-intersperse where-parts " AND "))
(print "WHERE clause: " where-clause)

(define filter-values (map cdr filters))
(print "Filter values: " filter-values)
(newline)

;; ========== EXAMPLE 5: Full SELECT with Filters ==========

(print "=== Example 5: Full SELECT with Filters ===")

(define (build-select-sql table-name filters)
  "Build SELECT SQL with WHERE clause"
  (let ((base (conc "SELECT * FROM " table-name)))
    (if (null? filters)
        base
        (let* ((where-parts
                 (map (lambda (filter)
                        (let ((field (string-translate
                                       (symbol->string (car filter))
                                       #\- #\_)))
                          (conc field " = ?")))
                      filters))
               (where-clause (string-intersperse where-parts " AND ")))
          (conc base " WHERE " where-clause)))))

(define sql1 (build-select-sql "comment" '()))
(print "No filters: " sql1)

(define sql2 (build-select-sql "comment" '((page-name . "blog_post"))))
(print "One filter: " sql2)

(define sql3 (build-select-sql "comment" '((page-name . "blog_post") (name . "Alice"))))
(print "Two filters: " sql3)
(newline)

;; ========== EXAMPLE 6: Putting It Together ==========

(print "=== Example 6: Complete Query Function ===")

(define (query-table table-name field-names filters)
  "Query a table and return alists"
  (let* ((sql-str (build-select-sql table-name filters))
         (stmt (sql db sql-str))
         (filter-values (map cdr filters))
         (rows (if (null? filter-values)
                   (query fetch-all stmt)
                   (apply query fetch-all stmt filter-values))))

    (print "SQL: " sql-str)
    (print "Values: " filter-values)
    (print "Rows: " (length rows))

    ;; Convert rows to alists
    (map (lambda (row) (row->alist field-names row))
         rows)))

(define results
  (query-table "comment"
               '(id name address page_name timestamp text)
               '((page-name . "welcome"))))

(print "Results:")
(for-each (lambda (r) (print "  " r)) results)

(close-database db)
