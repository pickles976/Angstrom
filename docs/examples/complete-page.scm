#!/usr/bin/env -S csi -s

;;; complete-page.scm - Complete example using all Angstrom features
;;;
;;; This demonstrates:
;;; - Micron DSL for layout
;;; - Markdown for content
;;; - ORM for data
;;; - Forms for interaction

(import micron)
(import markdown)
(import orm)

;; Load configuration
(load "app/settings.scm")

;; Initialize ORM
(orm-init (app-models-path))

;; === PAGE HEADER ===
(define (page-header)
  (conc
    (style '(align center))
    (section "Angstrom Demo")
    (style '(fg "888"))
    "A complete example page"
    (reset-style)
    (style '(align left))
    nl nl))

;; === CONTENT SECTION (from Markdown) ===
(define (content-section)
  (conc
    (subsection "About This Page")
    nl
    (markdown->micron "
This page demonstrates **all Angstrom features**:

- *Micron DSL* for styling and layout
- *Markdown conversion* for content
- *Database integration* with the ORM
- *Interactive forms* for user input

Everything works together seamlessly!")
    nl nl))

;; === DATABASE SECTION ===
(define (database-section)
  (conc
    (subsection "Recent Activity")
    nl

    ;; Open database and fetch recent comments
    (let* ((dummy (db-open (app-db-path)))
           (comments (db-list 'comment))
           (recent (if (> (length comments) 3)
                      (take comments 3)
                      comments)))

      (db-close)

      ;; Display results
      (if (null? recent)
          (conc (style '(fg "888"))
                "No activity yet. Be the first to comment!"
                (reset-style))
          (apply conc
            (map (lambda (comment)
                   (conc
                     (style '(fg "5af"))
                     (alist-ref 'name comment)
                     (reset-style)
                     " said: "
                     (style '(fg "ddd"))
                     (alist-ref 'text comment)
                     (reset-style)
                     nl))
                 recent))))
    nl nl))

;; === FORM SECTION ===
(define (form-section page-id)
  (conc
    (subsection "Leave a Comment")
    nl
    (style '(fg "888"))
    "Share your thoughts:"
    (reset-style)
    nl nl

    ;; Name input
    "Name: "
    (style '(bg "333" fg "fff"))
    (input-field-fixed "name" 20)
    (reset-style)
    nl

    ;; Comment input
    "Comment: "
    (style '(bg "333" fg "fff"))
    (input-field-fixed "text" 50)
    (reset-style)
    nl nl

    ;; Submit button
    (style '(bg "373" fg "fff"))
    (submit-field "Submit" "/app/actions/handle_comment.scm" page-id "name" "text")
    (reset-style)
    nl nl))

;; === FOOTER ===
(define (page-footer)
  (conc
    (divider)
    nl
    (style '(fg "888" align center))
    "Powered by Angstrom | "
    (link "/page/index.mu" "Home")
    (reset-style)))

;; === RENDER PAGE ===
(print
  (page-header)
  (content-section)
  (database-section)
  (form-section "demo")
  (page-footer))
