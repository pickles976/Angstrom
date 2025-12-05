#!/usr/bin/env -S csi -s

;;; orm.mu - ORM Guide

(import micron)
(load "~/.nomadnetwork/storage/pages/app/templates/comments.scm)
(define page-name "orm")

(print
  (style '(align center))
  (section "ORM Guide")
  (style '(align left))
  nl nl

  (subsection "What is the ORM?")
  nl
  (style '(fg "ddd"))
  "A simple Object-Relational Mapper for building data-driven Nomadnet apps."
  nl
  "Automatically generates database tables and provides easy CRUD operations."
  nl nl

  (subsection "Defining Models")
  nl
  "Models are defined as alists in " (code "app/models.scm") ":" nl nl

  (code "(define comment-model") nl
  (code "  '((name . comment)") nl
  (code "    (fields . (") nl
  (code "      ((name . id) (type . integer) (primary-key . #t))") nl
  (code "      ((name . text) (type . text))))))") nl nl

  (subsection "Generating Tables")
  nl
  "Create database tables from your models:" nl nl

  (code "csi -s framework/manage.scm --generate \\") nl
  (code "  --db-path /absolute/path/to/app.db \\") nl
  (code "  --models-path /absolute/path/to/models.scm") nl nl

  (subsection "Using the ORM")
  nl
  (bold "Initialize:") nl
  (code "(import orm)") nl
  (code "(load \"app/settings.scm\")") nl
  (code "(orm-init (app-models-path))") nl nl

  (bold "Open/Close Database:") nl
  (code "(db-open (app-db-path))") nl
  (code "(db-close)") nl nl

  (bold "Create (INSERT):") nl
  (code "(define comment (make-comment '((text . \"Hello\"))))") nl
  (code "(db-save comment)") nl nl

  (bold "Read (SELECT):") nl
  (code "(db-list 'comment)") "  ; Get all comments" nl
  (code "(db-list 'comment '((page-name . \"index\")))") "  ; Filter" nl nl

  (subsection "Working with Instances")
  nl
  "Instances are alists with a " (code "__model__") " key:" nl nl

  (code "(define comment (make-comment '((name . \"Alice\"))))") nl
  (code "(alist-ref 'name comment)") "  ; Get field value" nl nl

  (subsection "Raw SQL")
  nl
  "For complex queries, use " (code "sql-de-lite") " directly:" nl nl

  (code "(let* ((db (open-database path))") nl
  (code "       (stmt (sql db \"SELECT * FROM comment LIMIT ?\"))") nl
  (code "       (rows (query fetch-all stmt 10)))") nl
  (code "  (close-database db)") nl
  (code "  rows)") nl nl

  "See " (code "app/templates/recent-comments.scm") " for a complete example." nl nl

  (subsection "Key Concepts")
  nl
  (bold "Alists:") " Association lists for data" nl
  (code "'((name . \"Alice\") (age . 30))") nl nl

  (bold "Parameters:") " Thread-safe configuration" nl
  (code "(app-db-path)") " - Get configured database path" nl nl

  (bold "Auto Constructors:") " Generated from models" nl
  (code "(make-comment fields)") " - Created automatically" nl nl

  (subsection "Limitations")
  nl
  (style '(fg "888"))
  "The ORM is intentionally simple:" nl
  "- No UPDATE or DELETE" nl
  "- No JOINs" nl
  "- Only equality filters (no <, >, LIKE)" nl
  "- AND filters only (no OR)" nl nl
  "Use raw SQL for complex operations."
  (reset-style) nl nl

  (subsection "Learn More")
  nl
  (style '(fg "888"))
  "See " (code "framework/orm-lib.scm") " for the full implementation."
  (reset-style) nl nl
  (style '(fg "5af"))
  (link "/page/index.mu" "Back to Home")
  (reset-style)

  (display-comments app-db-path page-id))
