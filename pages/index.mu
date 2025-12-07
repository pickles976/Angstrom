#!/usr/bin/env -S csi -s

;;; index.mu - Angstrom Framework Homepage

(import micron)
(import (chicken process-context))
(import (chicken file))
(import (chicken port))

;; ========== DEBUG INFO ==========
;; Print debug information to stderr
; (with-output-to-port (current-error-port)
;   (lambda ()
;     (print "=== DEBUG INFO ===")
;     (print "Working Directory: " (current-directory))
;     (print "")
;     (print "Environment Variables:")
;     (for-each
;       (lambda (env-pair)
;         (print "  " (car env-pair) " = " (cdr env-pair)))
;       (get-environment-variables))
;     (print "==================")
;     (print "")))

;; Load settings (running from pages/, so app/ is relative)
(load "/home/sebas/Projects/NomadnetTemplates/pages/app/settings.scm")
(load "/home/sebas/Projects/NomadnetTemplates/pages/app/templates/header.scm")
(load "/home/sebas/Projects/NomadnetTemplates/pages/app/templates/comments.scm")
(load "/home/sebas/Projects/NomadnetTemplates/pages/app/templates/code.scm")

;; Configuration
(define page-name "index")

(define (my-input-field label fieldname size)
  (conc (style '(bg "333" fg "aaa")) (input-field-fixed fieldname size) (reset-style) label nl))

;; Generate the page
(print

  (Angstrom-header)

  ;; Welcome section
  (style '(align left))
  nl

    (style '(fg "ddd"))
    "This is a framework written (mostly by claude) to allow for the easy"
    nl
    "construction of " (bold "interactive pages") " on nomadnet."
    nl nl

    (style '(fg "5af"))
    "https://github.com/pickles976/Angstrom"
    (style '(fg "ddd"))
    nl nl

    "I found that there was not enough easy to use tooling around nomadnet"
    nl
    "and micron, and I hope that Angstrom helps bridge that gap."
    nl nl

    (style '(fg "5af"))
    (file-link "/file/Angstrom.tar.gz" "Download Angstrom.tar.gz")
    (style '(fg "ddd"))
    nl

  (style '(align left))
  nl
  (section "Demos")

  nl
  (style '(fg "5af"))
  (bold (file-link "/page/subpages/file-browser.mu" "File Browser"))
  nl nl

  (style '(fg "5af"))
  (bold (file-link "/page/subpages/recipe-search.mu" "Recipe Search"))
  nl nl

  ;; Installation instructions
  (style '(align left))
  nl
  (section "Installation")
  nl
  "To get Angstrom running on your system:"
  nl nl

  (subsection "Clone the repository")
  nl nl
  (code-block
  (code "git clone https://github.com/pickles976/Macro.git") nl
  (code "cd Macro") nl nl)

  (subsection "Install Chicken Scheme")
  nl nl
  "* On Debian/Ubuntu: " (code "sudo apt-get install chicken-bin") nl
  "* On Arch: " (code "sudo pacman -S chicken") nl
  "* On macOS: " (code "brew install chicken") nl nl

  (subsection "Install required Chicken Scheme packages")
  nl
  (code-block
  (code "sudo chicken-install sql-de-lite srfi-1 srfi-13 srfi-19 fmt") nl nl)

  (subsection "Build and install Angstrom modules")
  nl
  (code-block
  (code "cd pages/framework") nl nl
  (code "# Build the modules") nl
  (code "csc -s micron.scm") nl
  (code "csc -s markdown.scm") nl
  (code "csc -s orm-lib.scm") nl nl
  (code "# Install system-wide (optional)") nl
  (code "sudo chicken-install -s micron.egg") nl
  (code "sudo chicken-install -s markdown.egg") nl
  (code "sudo chicken-install -s orm.egg") nl nl
  (code "cd ../..") nl nl)

  (subsection "Deploy to Nomadnet")
  nl
  (code-block
  (code "# Copy pages to your Nomadnet storage directory") nl
  (code "cp -r pages/* ~/.nomadnetwork/storage/pages/") nl nl
  (code "# Make the main page executable") nl
  (code "chmod +x ~/.nomadnetwork/storage/pages/index.mu") nl nl)

  (subsection "Update Paths")
  nl nl
  "You will need to update the paths of the app. If you are using relative imports, make sure that you are running "
  (code "nomadnet") " from the proper directory." nl
  "I personally prefer to use absolute paths." nl nl

  (subsection "Generate the database tables")
  nl
  (code-block
  (code "cd ~/.nomadnetwork/storage/pages") nl
  (code "csi -s framework/manage.scm --generate") nl nl)

  "Your Angstrom site is now live on your Nomadnet node! Access it through the Nomadnet interface." nl nl

  nl
  (section "Reference")
  nl
  "Here is some documentation to get you started:"
  nl

  nl
  "If you just want to make a static page, or just aren't very familiar with micron, check out one of these projects:

https://fr33n0w.github.io/micron-composer/
https://rfnexus.github.io/micron-parser-js/
  "

  nl
  (style '(fg "5af"))
  (bold (file-link "/page/subpages/chicken-scheme-basics.mu" "Scheme programming basics"))
  nl nl
  (bold (file-link "/page/subpages/micron-dsl.mu" "Generate micron with scheme "))
  nl nl
  (bold (file-link "/page/subpages/orm.mu" "Simple ORM guide"))
  (style '(fg "ddd"))
  nl

  ;; Recent Comments section (uses raw SQL - see template for learning example)
  nl
  (section "Recent Comments (All Pages)")
  nl

    (style '(align left fg "ddd"))
    nl
    (display-recent-comments (app-db-path) 10)
    nl

  (subsection "Leave a Comment")
  nl

    (style '(fg "aaa" align left))
    nl
    (my-input-field  " Name " "user_name" 16) nl
    (my-input-field  " LXMF Address (optional)" "user_lxmf" 32) nl
    (my-input-field  " Comment " "comment_text" 64) nl

    (style '(bg "373"))
    (submit-field "Submit" "/app/actions/handle_comment.scm" page-name "user_name" "user_lxmf" "comment_text")
    (reset-style))
