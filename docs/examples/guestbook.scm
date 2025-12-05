#!/usr/bin/env -S csi -s

;;; guestbook.scm - Simple guestbook using Angstrom
;;;
;;; A minimal but complete example showing:
;;; - Database queries
;;; - Form handling
;;; - Basic styling

(import micron)
(import orm)

(load "app/settings.scm")
(orm-init (app-models-path))

;; Open database
(db-open (app-db-path))

;; === FETCH AND DISPLAY ENTRIES ===
(define entries (db-list 'comment '((page-name . "guestbook"))))

(print
  ;; Header
  (style '(align center))
  (section "Guestbook")
  (style '(align left))
  nl nl

  ;; Entries count
  (style '(fg "888"))
  "Total entries: " (length entries)
  (reset-style)
  nl nl

  ;; Display each entry
  (subsection "Messages")
  nl

  (if (null? entries)
      (conc (style '(fg "888")) "No entries yet. Be the first!" (reset-style) nl)
      (apply conc
        (map (lambda (entry)
               (conc
                 (style '(fg "5af"))
                 (alist-ref 'name entry)
                 (reset-style)
                 " (" (alist-ref 'timestamp entry) ")"
                 nl
                 (style '(fg "ddd"))
                 (alist-ref 'text entry)
                 (reset-style)
                 nl
                 (divider)
                 nl))
             (reverse entries))))  ; Show newest first

  nl

  ;; Sign guestbook form
  (subsection "Sign the Guestbook")
  nl

  "Your name: "
  (style '(bg "333" fg "fff"))
  (input-field-fixed "visitor_name" 24)
  (reset-style)
  nl nl

  "Your message: "
  (style '(bg "333" fg "fff"))
  (input-field-fixed "message" 60)
  (reset-style)
  nl nl

  (style '(bg "373" fg "fff"))
  (submit-field "Sign" "/app/actions/guestbook-sign.scm" "guestbook"
                "visitor_name" "message")
  (reset-style))

;; Close database
(db-close)

;; === HANDLER SCRIPT EXAMPLE ===
;;
;; Create app/actions/guestbook-sign.scm:
;;
;; #!/usr/bin/env -S csi -s
;;
;; (import orm)
;; (import (chicken process-context))
;; (import srfi-19)
;;
;; (load "app/settings.scm")
;; (orm-init (app-models-path))
;;
;; (define name (get-environment-variable "field_visitor_name"))
;; (define message (get-environment-variable "field_message"))
;; (define timestamp (date->string (current-date) "~Y-~m-~d ~H:~M"))
;;
;; (define entry
;;   (make-comment `((name . ,name)
;;                   (text . ,message)
;;                   (page-name . "guestbook")
;;                   (timestamp . ,timestamp)
;;                   (address . ""))))
;;
;; (db-open (app-db-path))
;; (db-save entry)
;; (db-close)
;;
;; (print "Thank you for signing the guestbook!")
;; (print (link "/page/guestbook.mu" "View guestbook"))
