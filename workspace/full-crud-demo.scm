#!/usr/bin/env -S csi -s

;;; full-crud-demo.scm - Complete Create + Read demonstration

(load "orm-lib.scm")

(print "╔════════════════════════════════════════╗")
(print "║   Full CRUD Demo (Create + Read)      ║")
(print "╚════════════════════════════════════════╝")
(newline)

(db-open "app.db")

;; ========== CREATE: Insert Data ==========

(print "━━━ CREATE: Adding new data ━━━")
(newline)

(print "1. Creating a new blog post...")
(define my-post
  (make-post
    '((title . "Getting Started with Nomadnet")
      (content . "Learn how to set up your first mesh node!")
      (author . "Bob")
      (created-at . "2025-12-04 19:00"))))
(db-save my-post)
(newline)

(print "2. Adding some comments...")
(define comment1
  (make-comment
    '((name . "Alice")
      (address . "abc123")
      (page-name . "getting-started")
      (timestamp . "2025-12-04 19:15")
      (text . "Great tutorial!"))))
(db-save comment1)

(define comment2
  (make-comment
    '((name . "Charlie")
      (address . "")
      (page-name . "getting-started")
      (timestamp . "2025-12-04 19:30")
      (text . "Very helpful, thanks!"))))
(db-save comment2)

(define comment3
  (make-comment
    '((name . "Alice")
      (address . "abc123")
      (page-name . "welcome")
      (timestamp . "2025-12-04 19:45")
      (text . "Welcome everyone!"))))
(db-save comment3)
(newline)

;; ========== READ: Query Data ==========

(print "━━━ READ: Querying data ━━━")
(newline)

(print "1. Get ALL posts:")
(define all-posts (db-list 'post))
(print "   Found " (length all-posts) " posts:")
(for-each
  (lambda (p)
    (print "     - " (alist-ref 'title p) " by " (alist-ref 'author p)))
  all-posts)
(newline)

(print "2. Get ALL comments:")
(define all-comments (db-list 'comment))
(print "   Found " (length all-comments) " comments total")
(newline)

(print "3. Get comments for 'getting-started' page:")
(define gs-comments (db-list 'comment '((page-name . "getting-started"))))
(print "   Found " (length gs-comments) " comments:")
(for-each
  (lambda (c)
    (print "     - " (alist-ref 'name c) ": \"" (alist-ref 'text c) "\""))
  gs-comments)
(newline)

(print "4. Get comments for 'welcome' page:")
(define welcome-comments (db-list 'comment '((page-name . "welcome"))))
(print "   Found " (length welcome-comments) " comments:")
(for-each
  (lambda (c)
    (print "     - " (alist-ref 'name c) ": \"" (alist-ref 'text c) "\""))
  welcome-comments)
(newline)

(print "5. Get all comments by Alice:")
(define alice-comments (db-list 'comment '((name . "Alice"))))
(print "   Found " (length alice-comments) " comments from Alice:")
(for-each
  (lambda (c)
    (print "     - On page '" (alist-ref 'page-name c) "': \"" (alist-ref 'text c) "\""))
  alice-comments)
(newline)

(print "6. Get Alice's comments on 'getting-started':")
(define alice-gs (db-list 'comment '((name . "Alice") (page-name . "getting-started"))))
(print "   Found " (length alice-gs) " comments:")
(for-each
  (lambda (c)
    (print "     - \"" (alist-ref 'text c) "\""))
  alice-gs)
(newline)

;; ========== WORKING WITH RESULTS ==========

(print "━━━ Working with query results ━━━")
(newline)

(print "7. Results are just alists - easy to work with!")
(define sample (car gs-comments))
(print "   Sample comment: " sample)
(print "   Author: " (alist-ref 'name sample))
(print "   Text: " (alist-ref 'text sample))
(print "   Timestamp: " (alist-ref 'timestamp sample))
(print "   Has __model__: " (alist-ref '__model__ sample))
(newline)

(print "8. You can filter/map/process results:")
(define comment-texts
  (map (lambda (c) (alist-ref 'text c))
       gs-comments))
(print "   All comment texts on getting-started:")
(for-each
  (lambda (text) (print "     - " text))
  comment-texts)
(newline)

(db-close)

(print "╔════════════════════════════════════════╗")
(print "║   Demo Complete!                       ║")
(print "╚════════════════════════════════════════╝")
(print "")
(print "Key takeaways:")
(print "  • CREATE: (db-save (make-comment ...))")
(print "  • READ ALL: (db-list 'comment)")
(print "  • READ FILTERED: (db-list 'comment '((field . value)))")
(print "  • Results are alists - easy to process!")
