#!/usr/bin/env -S csi -s

;;; orm.scm - Simple ORM for generating and working with SQLite tables
;;;
;;; Usage:
;;;   csi -s orm.scm --generate    ; Create all tables from models.scm

(import scheme)
(import (chicken base))
(import (chicken process-context))  ; For command-line args
(import (chicken string))           ; For string-intersperse
(import srfi-13)                    ; For string-upcase and other string ops
(import sql-de-lite)                ; SQLite support

;; ========== COMMAND-LINE ARGUMENT PARSING ==========

;;; In Chicken Scheme, (command-line-arguments) returns a list of strings
;;; For example: ["--generate"] or ["--help"]
;;;
;;; We'll use a simple approach: check if the list contains our flag

(define args (command-line-arguments))

;;; The 'member' function checks if an item is in a list
;;; It returns #f if not found, or the sublist starting at that element if found
;;; So (member "--generate" args) returns #f or a truthy value

(define generate? (member "--generate" args))
(define help? (member "--help" args))

;;; Helper to get argument value after a flag
;;; Example: For args like ["--generate" "--db-path" "/path/to/db.db"]
;;; (get-arg-value "--db-path") returns "/path/to/db.db"
(define (get-arg-value flag)
  (let ((flag-pos (member flag args)))
    (if (and flag-pos (not (null? (cdr flag-pos))))
        (cadr flag-pos)
        #f)))

;; Get database path from arguments or use default
(define db-path (or (get-arg-value "--db-path") "app/app.db"))

;; Get models path from arguments or use default
(define models-path (or (get-arg-value "--models-path") "app/models.scm"))

;; ========== WORKING WITH ALISTS ==========

;;; alist-ref is a built-in function that looks up a key in an association list
;;; Example: (alist-ref 'name '((name . "John") (age . 30))) returns "John"
;;;
;;; Let's create helper functions to extract model information

(define (model-name model)
  "Get the table name from a model"
  (alist-ref 'name model))

(define (model-fields model)
  "Get the list of fields from a model"
  (alist-ref 'fields model))

(define (field-name field)
  "Get the name of a field"
  (alist-ref 'name field))

(define (field-type field)
  "Get the SQL type of a field"
  (alist-ref 'type field))

(define (field-size field)
  "Get the optional size of a field (returns #f if not set)"
  (alist-ref 'size field))

(define (field-primary-key? field)
  "Check if field is a primary key"
  (alist-ref 'primary-key field))

(define (field-autoincrement? field)
  "Check if field auto-increments"
  (alist-ref 'autoincrement field))

;; ========== SQL GENERATION ==========

(define (field->sql field)
  "Convert a field definition to a SQL column definition string"
  ;;;
  ;;; This function builds a string like: "title TEXT(64)"
  ;;; or "id INTEGER PRIMARY KEY AUTOINCREMENT"
  ;;;
  ;;; We'll use 'conc' (concatenate) to build the string piece by piece

  (let* ((name (field-name field))
         (type (field-type field))
         (size (field-size field))

         ;; Convert name to string and replace hyphens with underscores
         ;; SQLite doesn't like hyphens in column names
         ;; 'string-translate' replaces characters: (string-translate "a-b" #\- #\_) -> "a_b"
         (name-str (string-translate (symbol->string name) #\- #\_))

         ;; Convert type symbol to uppercase string: 'text -> "TEXT"
         (type-str (string-upcase (symbol->string type)))

         ;; Add size if present: "TEXT" -> "TEXT(64)"
         (type-with-size
           (if size
               (conc type-str "(" size ")")
               type-str))

         ;; Build the base column definition
         (base (conc name-str " " type-with-size)))

    ;; Now add optional modifiers using nested 'if' or 'cond'
    ;; We'll chain the string building
    (let ((with-pk
            (if (field-primary-key? field)
                (conc base " PRIMARY KEY")
                base)))
      (if (field-autoincrement? field)
          (conc with-pk " AUTOINCREMENT")
          with-pk))))

(define (model->create-table-sql model)
  "Generate a CREATE TABLE SQL statement from a model"
  ;;;
  ;;; This will produce something like:
  ;;; "CREATE TABLE IF NOT EXISTS post (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT(64), ...)"
  ;;;
  ;;; We need to:
  ;;; 1. Convert each field to SQL using field->sql
  ;;; 2. Join them with commas
  ;;; 3. Wrap in CREATE TABLE statement

  (let* ((table-name (symbol->string (model-name model)))
         (fields (model-fields model))

         ;; 'map' applies a function to each element of a list
         ;; It's like [field->sql(f) for f in fields] in Python
         (field-sqls (map field->sql fields))

         ;; 'string-intersperse' joins strings with a separator
         ;; Like ", ".join(field_sqls) in Python
         (fields-str (string-intersperse field-sqls ", ")))

    (conc "CREATE TABLE IF NOT EXISTS "
          table-name
          " ("
          fields-str
          ")")))

;; ========== DATABASE OPERATIONS ==========

(define (generate-tables db-name)
  "Create database tables from models.scm"
  ;;;
  ;;; This function:
  ;;; 1. Loads models.scm to get model definitions
  ;;; 2. Generates CREATE TABLE SQL for each model
  ;;; 3. Executes the SQL using sql-de-lite

  ;; Load the models file
  ;; 'load' executes a Scheme file and makes its definitions available
  (print "Loading models from " models-path "...")
  (load models-path)

  ;; Now 'all-models' is available from models.scm
  (print "Found " (length all-models) " models")
  (newline)

  ;; Open the database
  (let ((db (open-database db-name)))

    ;; For each model, generate and execute CREATE TABLE
    ;; 'for-each' is like map, but used for side effects (no return value)
    (for-each
      (lambda (model)
        (let ((sql-statement (model->create-table-sql model))
              (table-name (symbol->string (model-name model))))

          ;; Print what we're doing
          (print "Creating table: " table-name)
          (print "SQL: " sql-statement)
          (newline)

          ;; Execute the SQL
          ;; 'exec' runs SQL that doesn't return results
          ;; 'sql' prepares a SQL statement from a string
          (exec (sql db sql-statement))))

      all-models)

    ;; Close the database
    (close-database db)
    (print "Done! Database created: " db-name)))

;; ========== HELP TEXT ==========

(define (show-help)
  (print "ORM - Simple Object-Relational Mapper for Nomadnet")
  (print "")
  (print "Usage:")
  (print "  csi -s manage.scm --generate [OPTIONS]")
  (print "    Generate database tables from models")
  (print "")
  (print "Options:")
  (print "  --db-path PATH       Absolute path to database file (default: app/app.db)")
  (print "  --models-path PATH   Absolute path to models file (default: app/models.scm)")
  (print "  --help               Show this help message")
  (print ""))

;; ========== MAIN LOGIC ==========

;;; The 'cond' form is Scheme's multi-way conditional (like switch/case)
;;; It tries each condition in order and executes the first one that's true

(cond
  (help?
    (show-help))

  (generate?
    (generate-tables db-path))

  (else
    (print "Error: Unknown command")
    (print "")
    (show-help)))