#!/usr/bin/env -S csi -s

;;; micron-dsl.scm - DSL for generating Nomad Network Micron markup

(import scheme)
(import chicken.base)      ; For export, make-parameter, parameterize
(import (chicken string))  ; For string-intersperse, string-null?, conc
(import srfi-1)           ; For alist-ref, assq

;; ========== CONSTANTS ==========
(define squiggle "-∿")
(define newline "  \n")

;; ========== HELPERS ============
(define (render-section depth title)
  (let ((heading (make-string depth #\>)))
    (conc heading " " title "\n")))

;; ========== RENDERERS ==========

(define (italics . inners) 
  (conc "`*" (apply conc inners) "`*"))

(define (bold . inners) 
  (conc "`!" (apply conc inners) "`!"))

(define (link src label)
  (conc "`[" label "`" src "]"))

(define (subsection . inners) (render-section 2 (apply conc inners)))
(define (section . inners) (render-section 1 (apply conc inners)))

(define (center . inners) (conc "`c" (apply conc inners) "`a"))

(define (input-field fieldname) 
  (conc "`<" fieldname "`>"))

(define (input-field-fixed fieldname size) 
  (conc "`<" size "|" fieldname "`>"))

(define (style attrs . body-elements)
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

;; Helper function to reset all styles
(define (reset-style) "``")

;; TODO: make this simpler
(define (submit-field label dest page . fields)
  (let ((field-str (string-intersperse fields "|")))
    (conc "`[" label "`:" dest "`" field-str "|post_id=" page "]")))
