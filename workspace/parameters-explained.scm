#!/usr/bin/env -S csi -s

;;; parameters-explained.scm - Understanding Chicken Scheme parameters
;;;
;;; Parameters are like global variables, but better:
;;; - Thread-safe
;;; - Can be dynamically scoped
;;; - Clean syntax for get/set

(import scheme)
(import (chicken base))
(import (chicken string))  ; For conc

(print "════════════════════════════════════════")
(print "  Understanding Parameters")
(print "════════════════════════════════════════")
(newline)

;; ========== EXAMPLE 1: The Problem with Global Variables ==========

(print "=== Example 1: Regular Global Variable ===")

(define global-db #f)

(define (open-db-v1 filename)
  (set! global-db (conc "Connected to: " filename))
  (print "Opened: " global-db))

(define (close-db-v1)
  (print "Closing: " global-db)
  (set! global-db #f))

(define (use-db-v1)
  (if global-db
      (print "Using: " global-db)
      (print "ERROR: No database!")))

(open-db-v1 "test.db")
(use-db-v1)
(close-db-v1)
(use-db-v1)
(newline)

;; ========== EXAMPLE 2: Parameters - Better Approach ==========

(print "=== Example 2: Using Parameters ===")

;;; make-parameter creates a parameter with an initial value
;;; A parameter is a special kind of function:
;;;   - Called with no args: GET the value
;;;   - Called with an arg: SET the value

(define db-connection (make-parameter #f))

(print "Initial value: " (db-connection))  ; GET: returns #f

;; Set a value
(db-connection "Connected to database")    ; SET

(print "After setting: " (db-connection))  ; GET: returns "Connected to database"

;; Set back to #f
(db-connection #f)                         ; SET

(print "After reset: " (db-connection))    ; GET: returns #f
(newline)

;; ========== EXAMPLE 3: Parameter Syntax Explained ==========

(print "=== Example 3: Parameter Syntax ===")

(define my-param (make-parameter 100))

(print "Syntax breakdown:")
(print "  (make-parameter 100)    ; Create parameter with initial value")
(print "  (my-param)              ; GET - no arguments returns value")
(print "  (my-param 200)          ; SET - one argument sets value")
(newline)

(print "Current value: " (my-param))        ; 100
(my-param 200)                              ; Set to 200
(print "New value: " (my-param))            ; 200
(newline)

;; ========== EXAMPLE 4: Parameters in Functions ==========

(print "=== Example 4: Parameters in Functions ===")

(define current-user (make-parameter #f))

(define (login username)
  (current-user username)
  (print "Logged in as: " (current-user)))

(define (logout)
  (print "Logging out: " (current-user))
  (current-user #f))

(define (get-username)
  (if (current-user)
      (current-user)
      "guest"))

(print "Current user: " (get-username))    ; guest
(login "alice")                             ; alice
(print "Current user: " (get-username))    ; alice
(logout)
(print "Current user: " (get-username))    ; guest
(newline)

;; ========== EXAMPLE 5: How Our ORM Uses It ==========

(print "=== Example 5: ORM Pattern ===")

;; This is what we do in orm-lib.scm
(define db (make-parameter #f))

(define (db-open filename)
  "Open database connection"
  (db (conc "DB: " filename))  ; In real code: (open-database filename)
  (print "Opened: " (db)))

(define (db-close)
  "Close database connection"
  (when (db)  ; when is like: if (db) then ...
    (print "Closing: " (db))
    (db #f)))

(define (db-query sql)
  "Execute a query"
  (unless (db)  ; unless is like: if (not (db)) then error
    (error "No database connection!"))
  (print "Executing on " (db) ": " sql))

;; Try it - this will error (intentionally)
(condition-case
  (db-query "SELECT * FROM users")  ; ERROR - no connection
  (ex () (print "ERROR caught: No database connection!")))

(print "")
(print "After opening:")
(db-open "mydb.sqlite")
(db-query "SELECT * FROM users")  ; Works!
(db-close)
(newline)

;; ========== EXAMPLE 6: Why This is Better ==========

(print "=== Example 6: Benefits ===")
(print "")
(print "1. Self-documenting:")
(print "   (db-connection)         ; clearly a getter")
(print "   (db-connection value)   ; clearly a setter")
(print "")
(print "2. No need for set!:")
(print "   (db-connection value)   ; instead of (set! db-connection value)")
(print "")
(print "3. Thread-safe:")
(print "   Multiple threads can have different parameter values")
(print "")
(print "4. Dynamic scoping with parameterize:")
(print "   (parameterize ((db-connection temp-db))")
(print "     ...)  ; db-connection temporarily changed")
(newline)

;; ========== EXAMPLE 7: Parameterize - Advanced ==========

(print "=== Example 7: Dynamic Scoping with parameterize ===")

(define log-level (make-parameter 'info))

(define (log msg)
  (print "[" (log-level) "] " msg))

(log "Normal message")  ; [info] Normal message

;; Temporarily change log level
(parameterize ((log-level 'debug))
  (log "Debug message")      ; [debug] Debug message
  (log "Another debug"))     ; [debug] Another debug

(log "Back to normal")  ; [info] Back to normal
(newline)

(print "════════════════════════════════════════")
(print "  Summary")
(print "════════════════════════════════════════")
(print "")
(print "Parameters are special procedures:")
(print "  - (param) with no args → GET value")
(print "  - (param value) with arg → SET value")
(print "  - Thread-safe and dynamically scopable")
(print "")
(print "In our ORM:")
(print "  (db-connection)           → Get current connection")
(print "  (db-connection db)        → Set connection")
(print "  (unless (db-connection)   → Check if connected")
(print "       (error ...))         → Raise error if not")
