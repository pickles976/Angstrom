#!/usr/bin/env -S csi -s

;;; micron-dsl.scm - DSL for generating Nomad Network Micron markup

(import scheme)
(import chicken.base)      ; For export, make-parameter, parameterize
(import (chicken string))  ; For string-intersperse, string-null?, conc
(import srfi-1)           ; For alist-ref, assq

;; ========== CONSTANTS ==========
(define squiggle "-∿")
(define newline "  \n")

;; ========== CORE RENDERERS ==========
(define (render element)
  (cond
    ((string? element) element) ; if string, return string
    ((pair? element) ; if pair, branch based on element
      (case (car element)
        ((micron) (apply conc (map render (cdr element)))) ; recurse
        ((center) (conc "`c" (apply conc (map render (cdr element))) "`a"))
        ((left) (conc "`a" (apply conc (map render (cdr element)))))
        ((right) (conc "`r" (apply conc (map render (cdr element))) "`a"))
        ((bold) (conc "`!" (apply conc (map render (cdr element))) "`!"))
        ((italics) (conc "`*" (apply conc (map render (cdr element))) "`*"))
        ((underline) (conc "`_" (apply conc (map render (cdr element))) "`_"))
        ((section) (render-section 1 (cdr element)))
        ((subsection) (render-section 2 (cdr element)))
        ((submit) (apply render-submit (cdr element)))
        ((field) (apply render-field (cdr element)))
        ((link) (apply render-link (cdr element)))
        ((style) (apply render-style (cdr element)))
        (else (apply conc (map render element)))))
    (else "")))

;; ========== COMPONENT RENDERERS ==========

(define (render-section depth title)
  (let ((heading (make-string depth #\>)))
    (conc heading " " title "\n")))

(define (render-field label name . args)
  (let ((size (if (null? args) 32 (car args)))
        (bg (assq 'bg (*current-styles*))))
    (conc (if bg (conc "`B" (cdr bg)) "")
          "`<" size "|" name "`>"
          (if bg "`B333" "")
          label newline)))

(define (input-field-fixed fieldname size) 
  (conc "`<" size "|" fieldname "`>"))


(define (render-submit label dest page . fields)
  (let ((field-str (string-intersperse fields "|")))
    (conc "`[" label "`:" dest "`" field-str "|post_id=" page "]")))

(define (render-link dest label)
  (conc "`[" label "`" dest "]"))

(define (render-style attrs . body-elements)
  (let loop ((attrs attrs) (output ""))
    (if (null? attrs)
        (conc output (apply conc body-elements))
        (let ((key (car attrs))
              (value (cadr attrs))
              (rest (cddr attrs)))
          (case key
            ((fg) (loop rest (conc output "`F" value)))
            ((bg) (loop rest (conc output "`B" value)))
            ((align) (case value
                      ((left) (loop rest (conc output "`a")))
                      ((center) (loop rest (conc output "`c")))
                      ((right) (loop rest (conc output "`r")))))
            (else (loop rest output)))))))

;; ========== HELPER FUNCTIONS ==========
(define (set-bg-color color) (conc "`B" color))
(define (set-fg-color color) (conc "`F" color))

;; Sugar syntax
(define (center . elements) (list 'center elements))
(define (bold . elements) (list 'bold elements))
(define (italics . elements) (list 'italics elements))
(define (section title) (list 'section title))
(define (subsection title) (list 'subsection title))
(define (field label name . size) (list 'field label name (if (null? size) 32 (car size))))
(define (submit label dest . args) (list 'submit label dest args))
(define (link dest label) (list 'link dest label))
(define (style attrs) (list 'style attrs))
(define (micron . elements) elements)