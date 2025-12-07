#!/usr/bin/csi -script

;;; recipe-search.mu - Recipe search interface for Nomadnet

(import (chicken process-context))
(import (chicken string))
(import (chicken irregex))
(import (chicken io))
(import micron)
(import http-client)
(import uri-common)
(import srfi-13)

(define page-name "recipe-search")

;; Get query parameter from environment or use default
(define query-param
  (or (get-environment-variable "var_query")
      ""))

;; Parse recipe items from HTML
;; Extracts: <a href="/recipe/?index=69175"><em>Crystallized Ginger</em></a>
(define recipe-pattern
  (irregex '(: "<a href=\"/recipe/?index=" (submatch (+ num)) "\">"
               "<em>" (submatch (+ (~ #\<))) "</em></a>")))

(define (extract-recipes html)
  "Parse HTML and return list of (index . title) pairs"
  (let loop ((matches (irregex-search recipe-pattern html))
             (results '()))
    (if matches
        (let ((index (irregex-match-substring matches 1))
              (title (irregex-match-substring matches 2)))
          (loop (irregex-search recipe-pattern html
                                (irregex-match-end-index matches))
                (cons (cons index title) results)))
        (reverse results))))

;; Only fetch recipes if query is not empty
(define recipes
  (if (string-null? query-param)
      '()
      (let ((html-response
              (with-input-from-request
                (string-append "https://hari.recipes/recipe_query/?query="
                               (uri-encode-string query-param))
                #f
                read-string)))
        (extract-recipes html-response))))

;; Render the page
(print
  (style '(align center fg "5af"))
  (bold "Recipe Search")
  nl nl

  (style '(align left fg "ddd"))
  "Search for recipes from http://hari.recipes"
  nl nl

  ;; Search form
  (style '(align left fg "aaa"))
  (input-field-fixed "query" 40)
  " Search query"
  nl nl

  (style '(bg "353" fg "fff"))
  (submit-field "Search" "/page/subpages/recipe-search.mu" page-name "query")
  (reset-style)
  nl nl

  ;; Only show results if query is not empty
  (if (not (string-null? query-param))
      (conc
        ;; Results header
        (style '(align left fg "5af"))
        (bold (string-append "Results for: \"" query-param "\""))
        nl nl

        ;; Recipe list as clickable submit-fields
        (style '(align left fg "ddd"))
        (apply conc
          (map (lambda (recipe)
                 (let ((index (car recipe))
                       (title (cdr recipe)))
                   (conc
                     (style '(bg "333" fg "eee"))
                     (submit-field title "/page/subpages/recipe-detail.mu" page-name
                                 (string-append "recipe_index=" index))
                     (reset-style)
                     nl)))
               recipes)))
      ""))