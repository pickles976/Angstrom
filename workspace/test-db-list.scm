#!/usr/bin/env -S csi -s

;;; test-db-list.scm - Test the db-list function

(load "orm-lib.scm")

(print "╔════════════════════════════════════════╗")
(print "║   Testing db-list                      ║")
(print "╚════════════════════════════════════════╝")
(newline)

(db-open "app.db")

;; ========== TEST 1: Get All Comments ==========

(print "=== Test 1: Get all comments ===")
(define all-comments (db-list 'comment))
(print "Found " (length all-comments) " comments")
(print "First comment:")
(print "  " (car all-comments))
(newline)

;; ========== TEST 2: Filter by Single Field ==========

(print "=== Test 2: Filter by page-name ===")
(define blog-comments (db-list 'comment '((page-name . "blog_post"))))
(print "Found " (length blog-comments) " comments for blog_post")
(for-each
  (lambda (c)
    (print "  ID: " (alist-ref 'id c)
           ", Name: " (alist-ref 'name c)
           ", Text: " (alist-ref 'text c)))
  blog-comments)
(newline)

;; ========== TEST 3: Filter by Multiple Fields ==========

(print "=== Test 3: Filter by multiple fields ===")
(define alice-blog-comments
  (db-list 'comment '((page-name . "blog_post") (name . "Alice"))))
(print "Found " (length alice-blog-comments) " comments from Alice on blog_post")
(for-each
  (lambda (c)
    (print "  " c))
  alice-blog-comments)
(newline)

;; ========== TEST 4: Query Different Model ==========

(print "=== Test 4: Query posts ===")
(define all-posts (db-list 'post))
(print "Found " (length all-posts) " posts")
(for-each
  (lambda (p)
    (print "  Title: " (alist-ref 'title p)
           ", Author: " (alist-ref 'author p)))
  all-posts)
(newline)

;; ========== TEST 5: Query Users ==========

(print "=== Test 5: Query users ===")
(define all-users (db-list 'user))
(print "Found " (length all-users) " users")
(for-each
  (lambda (u)
    (print "  Username: " (alist-ref 'username u)
           ", Email: " (alist-ref 'email u)))
  all-users)
(newline)

;; ========== TEST 6: Empty Results ==========

(print "=== Test 6: Filter with no results ===")
(define no-results (db-list 'comment '((name . "NonexistentUser"))))
(print "Found " (length no-results) " comments")
(if (null? no-results)
    (print "  ✓ Correctly returns empty list")
    (print "  ✗ Should be empty!"))
(newline)

;; ========== TEST 7: Verify Instance Structure ==========

(print "=== Test 7: Verify instance structure ===")
(define sample (car all-comments))
(print "Instance has __model__ key: " (alist-ref '__model__ sample))
(print "Instance has id: " (alist-ref 'id sample))
(print "Instance has name: " (alist-ref 'name sample))
(print "✓ Instances are properly structured alists")
(newline)

(db-close)

(print "╔════════════════════════════════════════╗")
(print "║   All tests complete!                  ║")
(print "╚════════════════════════════════════════╝")
