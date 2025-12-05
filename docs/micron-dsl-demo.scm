#!/usr/bin/env -S csi -s

;;; micron-dsl-demo.scm - Demonstration of all micron-dsl features

(load "framework/micron.scm")

(print "╔════════════════════════════════════════╗")
(print "║   Micron DSL - Complete Demo          ║")
(print "╚════════════════════════════════════════╝")
(print "")
(print "Run this to see micron markup generated.")
(print "Copy output to a .mu file to view in Nomadnet.")
(print "")
(print "═══════════════════════════════════════════")
(print "")

;; Generate a complete micron page
(print
  ;; Header
  squiggle nl
  (heading "Micron DSL Feature Demo") nl
  squiggle nl nl

  ;; Text Formatting
  (section "Text Formatting")
  (left)
  "Normal text" nl
  (bold "Bold text") nl
  (italics "Italic text") nl
  (underline "Underlined text") nl
  (bold-italic "Bold and italic") nl
  (bold-underline "Bold and underlined") nl
  nl

  ;; Colors
  (section "Colors")
  (left)
  (fg color-red "Red text") nl
  (fg color-green "Green text") nl
  (fg color-blue "Blue text") nl
  (bg color-yellow "Yellow background") nl
  (colored color-white color-blue "White on blue") nl
  (reset-colors) "Back to normal" nl
  nl

  ;; Semantic colors
  (subsection "Semantic Colors")
  (left)
  (success-text "Success message") nl
  (error-text "Error message") nl
  (warning-text "Warning message") nl
  (info-text "Info message") nl
  (muted-text "Muted/secondary text") nl
  nl

  ;; Alignment
  (section "Alignment")
  (left "Left-aligned text") nl
  (center "Centered text") nl
  (right "Right-aligned text") nl
  (align-default) nl

  ;; Sections
  (section "Section Nesting")
  (left)
  "This is in a level 1 section" nl
  (subsection "Level 2")
  "Inside level 2 section" nl
  (subsubsection "Level 3")
  "Inside level 3 section" nl
  nl

  ;; Dividers
  (section "Dividers")
  (left)
  "Content above divider" nl
  (divider)
  "Content below divider" nl
  nl

  ;; Links
  (section "Links")
  (left)
  (link "lxmf@exampleaddress" "LXMF Link") nl
  (link "http://example.com" "HTTP Link") nl
  (link "/page/other.mu" "Internal Page") nl
  (link-button "/page/action.mu" "Button-styled Link") nl
  nl

  ;; Literal/Code
  (section "Literal Blocks")
  (left)
  "Code example: " (code "`!this won't be bold`!") nl
  "Literal text: " (literal "special `chars kept as-is") nl
  nl

  ;; Comments
  (comment "This is a hidden comment - won't appear in rendered output")

  ;; Form inputs
  (section "Form Inputs")
  (left)

  (subsection "Text Inputs")
  (input-field "username") " Username (variable width)" nl
  (input-field-fixed "email" 32) " Email (32 chars)" nl
  (input-password "password" 20) " Password (masked)" nl
  nl

  (subsection "Checkboxes")
  (checkbox "opt1" "Option 1") nl
  (checkbox "opt2" "Option 2") nl
  (checkbox "opt3" "Option 3") nl
  nl

  (subsection "Radio Buttons")
  (radio "size" "small" "Small") nl
  (radio "size" "medium" "Medium") nl
  (radio "size" "large" "Large") nl
  nl

  (subsection "Submit Button")
  (submit-field "Submit Form" "/app/actions/handler.scm" "form-page"
                "username" "email" "password" "opt1" "opt2" "size")
  nl nl

  ;; Style combinations
  (section "Style Combinations")
  (left)
  (style '(fg "f0f" bg "333") "Magenta on dark gray") nl
  (style '(fg "0ff" align center) "Centered cyan text") nl
  (style '(bg "300" fg "fa0") "Orange on dark red") nl
  nl

  ;; Utilities
  (section "Utilities")
  (left)
  "Normal text" nl
  (spacer 2)
  "Text after 2 blank lines" nl
  (indent 0 "No indent") nl
  (indent 1 "Indent level 1") nl
  (indent 2 "Indent level 2") nl
  nl

  ;; Presets
  (section "Color Presets")
  (left)
  "Available color constants:" nl
  (fg color-red "color-red") " | "
  (fg color-green "color-green") " | "
  (fg color-blue "color-blue") nl
  (fg color-yellow "color-yellow") " | "
  (fg color-cyan "color-cyan") " | "
  (fg color-magenta "color-magenta") nl
  (fg color-white "color-white") " | "
  (fg color-gray "color-gray") " | "
  (fg color-black "color-black") nl
  nl

  ;; Footer
  squiggle nl
  (center (muted-text "End of demo")) nl
  squiggle
)
