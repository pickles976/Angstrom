#!/usr/bin/env -S csi -s

;;; parameter-interactive.scm
;;; A simple, concrete example showing parameters in action

(import scheme)
(import (chicken base))
(import (chicken string))

;; Create a parameter to hold a "database connection"
;; (We'll fake it with a string for simplicity)
(define db-connection (make-parameter #f))

(print "╔════════════════════════════════════════╗")
(print "║  Parameter Interactive Demo           ║")
(print "╚════════════════════════════════════════╝")
(newline)

(print "Step 1: Check initial state")
(print "  (db-connection) = " (db-connection))
(newline)

(print "Step 2: Set a value (simulating opening a database)")
(print "  Running: (db-connection \"SQLite:app.db\")")
(db-connection "SQLite:app.db")
(print "  Now (db-connection) = " (db-connection))
(newline)

(print "Step 3: Use the value")
(print "  We can check if connected:")
(print "    (if (db-connection)")
(print "        \"Connected!\"")
(print "        \"Not connected\")")
(print "  Result: " (if (db-connection) "Connected!" "Not connected"))
(newline)

(print "Step 4: Use it in a function")

(define (execute-query sql)
  (if (db-connection)
      (conc "Executing on [" (db-connection) "]: " sql)
      "ERROR: No database!"))

(print "  Defined function execute-query")
(print "  Running: (execute-query \"SELECT * FROM users\")")
(print "  Result: " (execute-query "SELECT * FROM users"))
(newline)

(print "Step 5: Reset to #f (simulating closing)")
(print "  Running: (db-connection #f)")
(db-connection #f)
(print "  Now (db-connection) = " (db-connection))
(newline)

(print "Step 6: Try using it now")
(print "  Running: (execute-query \"SELECT * FROM posts\")")
(print "  Result: " (execute-query "SELECT * FROM posts"))
(newline)

(print "╔════════════════════════════════════════╗")
(print "║  Key Takeaways                         ║")
(print "╚════════════════════════════════════════╝")
(print "")
(print "1. (db-connection) is a FUNCTION")
(print "2. Called with NO args → GET value")
(print "3. Called with ONE arg → SET value")
(print "4. Acts like a global variable but cleaner")
(print "")
(print "In orm-lib.scm:")
(print "  - db-open calls (db-connection <handle>)")
(print "  - db-save reads (db-connection) to use it")
(print "  - db-close resets (db-connection #f)")
