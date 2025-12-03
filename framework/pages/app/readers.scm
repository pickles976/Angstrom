#!/usr/bin/env -S csi -s

(import (chicken string))  ; For string-intersperse, string-null?, conc
(import (chicken file)) 
(import (chicken io)) 

(load "./framework/micron-dsl.scm")

;; ========== COMMENTS DISPLAY ==========

(define (parse-comment line)
    (handle-exceptions exn
        "" ; Skip malformed lines
        (let* ((parts (string-split comment "|" #t 4))
            (name (list-ref parts 0))
            (lxmf (list-ref parts 1))
            (timestamp (list-ref parts 2))
            (text (if (< (length parts) 4) "" (list-ref parts 3))))
        (string-append 
        (style '(fg "eee")) name (reset-style) 
        " (" timestamp "):" newline
        (if (and lxmf (not (string-null? lxmf)))
            (string-append 
                (style '(fg "0FD")) 
                (link (string-append "lxmf@" lxmf) (string-append "lxmf@" lxmf))
                (reset-style) newline)
            "")
        text newline "-" newline))))

;; TODO: FIX THIS
(define (display-comments comments-dir post-id)
    (let* ((comments-file (conc comments-dir "/" post-id ".txt")))
        (if (file-exists? comments-file)
            (let* ((lines (call-with-input-file comments-file (lambda (port) (read-lines port))))
                (reversed-comments (reverse lines)))
            (if (null? reversed-comments)
                "No comments yet. Be the first!"
                (conc
                (map parse-comment reversed-comments))))
            (conc "No comments file found at: " comments-file))))

;; Helper functions
(define (read-lines)
  (let loop ((line (read-line)) (lines '()))
    (if (eof-object? line)
        (reverse lines)
        (loop (read-line) (cons line lines)))))

(define (reset-style) "``")