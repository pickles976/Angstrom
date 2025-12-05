#!/usr/bin/env -S csi -s

;;; test-markdown-converter.scm - Test the markdown to micron converter
;;;
;;; Usage: cd pages && csi -s ../docs/test-markdown-converter.scm

(import scheme)
(import (chicken base))
(import (chicken string))
(import (chicken io))
(import (chicken file))

;; Load the markdown converter (which also loads micron-dsl)
(load "framework/markdown.scm")

;; ========== TEST UTILITIES ==========

(define (print-section title)
  "Print a section header"
  (print "\n" (make-string 60 #\=))
  (print title)
  (print (make-string 60 #\=) "\n"))

(define (test-conversion name input-file)
  "Test converting a markdown file and display results"
  (print-section (conc "Testing: " name))

  (print "Input file: " input-file)
  (print "\nConverted output:\n")
  (print (make-string 60 #\-))

  (let ((output (markdown-file->micron input-file)))
    (print output)
    (print (make-string 60 #\-))
    (print "\n")

    ;; Return the output for saving
    output))

(define (save-output name output)
  "Save converted output to a .mu file"
  (let ((output-file (conc "../docs/" name ".mu")))
    (call-with-output-file output-file
      (lambda (port)
        (display output port)))
    (print "Saved to: " output-file)))

;; ========== INLINE TESTS ==========

(define (test-inline)
  "Test inline formatting conversions"
  (print-section "Inline Formatting Tests")

  (define tests
    '(("Bold" "This is **bold text**")
      ("Italic" "This is *italic text*")
      ("Code" "This is `inline code`")
      ("Link" "Check [this link](https://example.com)")
      ("Mixed" "**Bold** and *italic* and `code`")
      ("Nested" "**Bold with *nested italic* inside**")))

  (for-each
    (lambda (test)
      (let ((name (car test))
            (input (cadr test)))
        (print "\n" name ":")
        (print "  Input:  " input)
        (print "  Output: " (markdown->micron input))))
    tests))

;; ========== MAIN TESTS ==========

(define (main)
  "Run all tests"
  (print "\n")
  (print "╔════════════════════════════════════════════════════════════╗")
  (print "║       Markdown to Micron Converter Test Suite            ║")
  (print "╚════════════════════════════════════════════════════════════╝")

  ;; Test inline conversions
  (test-inline)

  ;; Test simple example
  (let ((output1 (test-conversion "Simple Example" "../docs/example-simple.md")))
    (save-output "example-simple-converted" output1))

  ;; Test comprehensive example
  (let ((output2 (test-conversion "Comprehensive Test" "../docs/test-markdown.md")))
    (save-output "test-markdown-converted" output2))

  ;; Summary
  (print-section "Test Complete")
  (print "✓ Inline formatting tests completed")
  (print "✓ Simple example converted")
  (print "✓ Comprehensive test converted")
  (print "\nOutput files created in ../docs/ directory")
  (print "View them in Nomadnet or inspect manually\n"))

;; Run tests
(main)
