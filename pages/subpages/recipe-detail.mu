#!/usr/bin/csi -script

;;; recipe-detail.mu - Display a single recipe from hari.recipes

(import (chicken process-context))
(import (chicken string))
(import (chicken irregex))
(import (chicken io))
(import micron)
(import http-client)
(import uri-common)

(define page-name "recipe-detail")

;; Get recipe index from environment variable
(define recipe-index
  (or (get-environment-variable "var_recipe_index")
      "69175"))  ; default to first Crystallized Ginger recipe

;; Fetch recipe HTML from API
(define recipe-html
  (with-input-from-request
    (string-append "https://hari.recipes/recipe/?index=" recipe-index)
    #f
    read-string))

;; Parse recipe title (skip first h1 which is site header, get second h1)
(define (extract-title html)
  (let* ((first-match (irregex-search '(: "<h1>" (submatch (* (~ #\<))) "</h1>") html))
         (second-match (if first-match
                           (irregex-search '(: "<h1>" (submatch (* (~ #\<))) "</h1>")
                                         html
                                         (irregex-match-end-index first-match))
                           #f)))
    (if second-match
        (irregex-match-substring second-match 1)
        "Unknown Recipe")))

;; Parse source link
(define (extract-source-link html)
  (let ((match (irregex-search '(: "<a href=\"" (submatch (: "http" (* (~ #\")))) "\">Source Link</a>") html)))
    (if match
        (irregex-match-substring match 1)
        "")))

;; Parse ingredients list
(define (extract-ingredients html)
  (let* ((ingredients-section-match
          (irregex-search '(: "<h4>Ingredients:</h4>" (* space) "<ul>" (submatch (*? any)) "</ul>") html))
         (ingredients-html (if ingredients-section-match
                               (irregex-match-substring ingredients-section-match 1)
                               "")))
    (let loop ((matches (irregex-search '(: "<li>" (* space) "<em>" (submatch (+ (~ #\<))) "</em>") ingredients-html))
               (results '()))
      (if matches
          (loop (irregex-search '(: "<li>" (* space) "<em>" (submatch (+ (~ #\<))) "</em>")
                                ingredients-html
                                (irregex-match-end-index matches))
                (cons (irregex-match-substring matches 1) results))
          (reverse results)))))

;; Parse instructions list
(define (extract-instructions html)
  (let* ((instructions-section-match
          (irregex-search '(: "<h4>Instructions:</h4>" (* space) "<ul>" (submatch (*? any)) "</ul>") html))
         (instructions-html (if instructions-section-match
                                (irregex-match-substring instructions-section-match 1)
                                "")))
    (let loop ((matches (irregex-search '(: "<li>" (* space) "<em>" (submatch (+ (~ #\<))) "</em>") instructions-html))
               (results '()))
      (if matches
          (loop (irregex-search '(: "<li>" (* space) "<em>" (submatch (+ (~ #\<))) "</em>")
                                instructions-html
                                (irregex-match-end-index matches))
                (cons (irregex-match-substring matches 1) results))
          (reverse results)))))

(define recipe-title (extract-title recipe-html))
(define source-link (extract-source-link recipe-html))
(define ingredients (extract-ingredients recipe-html))
(define instructions (extract-instructions recipe-html))

;; Render the page
(print
  ;; Back button
  (style '(align left bg "533" fg "fff"))
  (submit-field "← Back to Search" "/page/subpages/recipe-search.mu" page-name)
  (reset-style)
  nl nl

  ;; Recipe title
  (style '(align center fg "5af"))
  (bold recipe-title)
  nl nl

  ;; Ingredients section
  (style '(align left fg "f83"))
  (bold "Ingredients:")
  nl nl

  (style '(align left fg "ddd"))
  (apply conc
    (map (lambda (ingredient)
           (conc "• " ingredient nl))
         ingredients))
  nl

  ;; Instructions section
  (style '(align left fg "f83"))
  (bold "Instructions:")
  nl nl

  (style '(align left fg "ddd"))
  (let loop ((steps instructions)
             (num 1)
             (result ""))
    (if (null? steps)
        result
        (loop (cdr steps)
              (+ num 1)
              (conc result num ". " (car steps) nl nl))))

  ;; Source link
  (style '(align left fg "aaa"))
  "Source: " nl
  (style '(fg "5af"))
  source-link
  nl)
