#!/usr/bin/env -S csi -s

;;; settings.scm - Application configuration
;;;
;;; Set your absolute paths here. No magic path finding - just simple configuration.

;; ========== USER CONFIGURATION ==========

;; Absolute path to your SQLite database file
;; Example: "/home/user/NomadnetTemplates/pages/app/app.db"
(define db-path "/home/sebas/Projects/NomadnetTemplates/pages/app/app.db")

;; Absolute path to your models file
;; Example: "/home/user/NomadnetTemplates/pages/app/models.scm"
(define models-path "/home/sebas/Projects/NomadnetTemplates/pages/app/models.scm")

;; ========== EXPORTS ==========

;; Export as parameters for clean access
(define app-db-path (make-parameter db-path))
(define app-models-path (make-parameter models-path))
