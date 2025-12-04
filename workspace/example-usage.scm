#!/usr/bin/env -S csi -s

;;; example-usage.scm - Example of using the ORM

(load "orm-lib.scm")

;; Create a comment instance
(define my-comment
  (make-comment
    '((name . "Alice")
      (address . "34dba26ea2b9d6ff1b8b55a347f8f083")
      (page-name . "blog_post")
      (timestamp . "2025-12-04 16:45")
      (text . "Great post! Thanks for sharing."))))

;; Create another comment
(define another-comment
  (make-comment
    '((name . "Bob")
      (address . "")
      (page-name . "index")
      (timestamp . "2025-12-04 17:00")
      (text . "Hello from Bob!"))))

;; Create a post instance
(define my-post
  (make-post
    '((title . "My First Post")
      (content . "This is the content of my first blog post.")
      (author . "Alice")
      (created-at . "2025-12-04 16:30"))))

;; Open database
(db-open "app.db")

;; Save instances
(print "Saving comments and post...")
(db-save my-comment)
(db-save another-comment)
(db-save my-post)

;; Close database
(db-close)

(print "\nDone! Check results:")
(print "  sqlite3 app.db 'SELECT * FROM comment;'")
(print "  sqlite3 app.db 'SELECT * FROM post;'")
