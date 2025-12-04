#!/usr/bin/env -S csi -s

(import coops)
(import sql-de-lite)
(import (chicken string))
(import srfi-19)

(define-class <comment> ()
  ((name initform: "" accessor: comment-name)
   (address initform: "" accessor: comment-address)
   (page-name initform: "" accessor: comment-page-name)
   (timestamp initform: (date->string (current-date) "~Y-~m-~d ~H:~M") accessor: comment-timestamp)
   (text initform: "" accessor: comment-text)))

(define db-name "comments.sqlite3")


;; Example lxmf address:
;; 34db a26e a2b9 d6ff 1b8b 55a3 47f8 f083

(define (create-db)
  (let ((db (open-database db-name)))
    (exec (sql db "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT(16), address TEXT(32), page_name TEXT(32), timestamp TEXT(16), text TEXT)"))
    (close-database db)))

(define (insert-comment comment)
  (declare (type <comment> comment))
  (let ((db (open-database db-name)))
    (exec (sql db "INSERT INTO comments (name, address, page_name, timestamp, text) VALUES (?,?,?,?,?)") 
      (comment-name comment)
      (comment-address comment)
      (comment-page-name comment)
      (comment-timestamp comment)
      (comment-text comment))
    (let ((id (last-insert-rowid db)))
      (close-database db)
      id)))

(define (get-comment id)
  (let ((db (open-database db-name)))
    (let ((result (query fetch (sql db "SELECT name, address, page_name, timestamp, text FROM comments WHERE id = ?;") id)))
      (close-database db)
      (if (null? result) #f result))))
  
;; Returns -- list of list of strings
(define (get-comments-for-page page_id)
  (let ((db (open-database db-name)))
    (let ((result (query fetch-all (sql db "SELECT name, address, page_name, timestamp, text FROM comments WHERE page_name = ?;") page_id)))
      (close-database db)
      (if (null? result) #f result))))


(define alice-comment
  (make <comment>
        'name "Alice"
        'page-name "index"
        'address "34dba26ea2b9d6ff1b8b55a347f8f083"
        'text "Fuck you"))

;; Use the three functions
(create-db)                               ; Creates table
(print (conc "Inserted ID: " (insert-comment alice-comment)))
(print (conc "Retrieved comment: " (get-comment 1)))
(print (conc "Comments for page: " 
  (string-intersperse 
    (map 
      (lambda (item) (conc (string-intersperse item " ")))
      (get-comments-for-page "index")) 
    "\n")))