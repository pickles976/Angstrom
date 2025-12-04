#!/usr/bin/env -S csi -s

;;; nomadnet-page-example.scm
;;; Example: A Nomadnet page that displays comments

(load "orm-lib.scm")

;; This would be your page handler in Nomadnet
;; The page-id comes from the URL or request

(define (render-comment-page page-id)
  "Render a page with its comments"

  (db-open "app.db")

  ;; Get comments for this specific page
  (define page-comments (db-list 'comment `((page-name . ,page-id))))

  (print "`c`!Comments for: " page-id "`!`a")
  (print "")
  (print "-∿-∿-∿-∿-∿-∿-∿-∿-∿-∿")
  (print "")

  (if (null? page-comments)
      (print "No comments yet. Be the first!")
      (for-each
        (lambda (comment)
          (let ((name (alist-ref 'name comment))
                (text (alist-ref 'text comment))
                (timestamp (alist-ref 'timestamp comment))
                (address (alist-ref 'address comment)))

            ;; Render each comment
            (print "`F33f" name "`f")
            (when (and address (not (string-null? address)))
              (print "  `F0aa(lxmf@" address ")`f"))
            (print "  `Faaa" timestamp "`f")
            (print "")
            (print text)
            (print "")
            (print "-")
            (print "")))
        page-comments))

  (db-close))

;; ========== DEMO ==========

(print "╔════════════════════════════════════════╗")
(print "║  Nomadnet Page Example                ║")
(print "╚════════════════════════════════════════╝")
(print "")

;; Render different pages
(render-comment-page "welcome")
(print "")
(print "════════════════════════════════════════")
(print "")
(render-comment-page "getting-started")
(print "")
(print "════════════════════════════════════════")
(print "")
(render-comment-page "nonexistent-page")
