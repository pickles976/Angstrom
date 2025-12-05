#!/usr/bin/env -S csi -s

;;; header.scm - Reusable header template for Angstrom pages

(import (chicken string))
(import micron)

;; ========== HEADER COMPONENT ==========

(define page-title "
 ________  ________   ________  ________  _________  ________  ________  _____ ______      
|\   __  \|\   ___  \|\   ____\|\   ____\|\___   ___\\   __  \|\   __  \|\   _ \  _   \    
\ \  \|\  \ \  \\ \  \ \  \___|\ \  \___|\|___ \  \_\ \  \|\  \ \  \|\  \ \  \\\__\ \  \   
 \ \   __  \ \  \\ \  \ \  \  __\ \_____  \   \ \  \ \ \   _  _\ \  \\\  \ \  \\|__| \  \  
  \ \  \ \  \ \  \\ \  \ \  \|\  \|____|\  \   \ \  \ \ \  \\  \\ \  \\\  \ \  \    \ \  \ 
   \ \__\ \__\ \__\\ \__\ \_______\____\_\  \   \ \__\ \ \__\\ _\\ \_______\ \__\    \ \__\
    \|__|\|__|\|__| \|__|\|_______|\_________\   \|__|  \|__|\|__|\|_______|\|__|     \|__|
")


(define (page-header #!optional (title "Angstrom") (subtitle "Nomadnet app framework"))
  "Generate a styled header with title and subtitle

   Parameters:
     title    - Main title text (default: 'Angstrom')
     subtitle - Subtitle text (default: 'Tools for building Nomadnet apps')

   Returns:
     Micron markup string for the header"
  (conc
    squiggle nl
    "`c`!`F3af" title "`!" nl
    "`*" subtitle "`*" nl
    "`Fddd" nl
    squiggle nl))

(define (Angstrom-header)
  "Generate the default Angstrom framework header"
  (page-header page-title "Nomadnet app framework"))
