#!/usr/bin/env -S csi -s

;;; micron-dsl.mu - Micron DSL Reference

(import micron)

(print
  (style '(align center))
  (section "Micron DSL")
  (style '(align left))
  nl nl

  (subsection "What is Micron?")
  nl
  (style '(fg "ddd"))
  "Micron is Nomadnet's markup language for styling terminal pages."
  nl
  "The Micron DSL lets you generate micron markup using Scheme functions."
  nl nl

  (subsection "Text Styling")
  nl
  (style '(fg "5af"))
  (bold "Bold text") (reset-style) " - " (code "(bold \"text\")") nl
  (style '(fg "fa5"))
  (italic "Italic text") (reset-style) " - " (code "(italic \"text\")") nl
  (style '(fg "5f5"))
  (underline "Underlined") (reset-style) " - " (code "(underline \"text\")") nl nl

  (subsection "Colors")
  nl
  "Set foreground and background colors:" nl nl
  (style '(fg "f00")) "Red text" (reset-style) " - " (code "(style '(fg \"f00\"))") nl
  (style '(bg "0f0" fg "000")) "Green background" (reset-style) " - " (code "(style '(bg \"0f0\" fg \"000\"))") nl
  (reset-style) nl

  (subsection "Headers")
  nl
  (code "(section \"Title\")") " - Top-level section" nl
  (code "(subsection \"Subtitle\")") " - Sub-section" nl nl

  (subsection "Links")
  nl
  "Create clickable links:" nl nl
  (style '(fg "5af"))
  (link "https://example.com" "External Link")
  (reset-style) nl
  (code "(link \"https://example.com\" \"External Link\")") nl nl

  (style '(fg "5af"))
  (link "/page/index.mu" "Internal Page")
  (reset-style) nl
  (code "(link \"/page/index.mu\" \"Internal Page\")") nl nl

  (subsection "Forms")
  nl
  "Interactive input fields:" nl nl
  (style '(bg "333" fg "aaa"))
  (input-field-fixed "username" 16)
  (reset-style) " Username" nl
  (code "(input-field-fixed \"username\" 16)") nl nl

  "Submit button:" nl
  (style '(bg "373"))
  (submit-field "Submit" "/app/actions/handler.scm" "page-id" "username")
  (reset-style) nl
  (code "(submit-field \"Submit\" \"/path\" \"page-id\" \"field1\" \"field2\")") nl nl

  (subsection "Alignment")
  nl
  (style '(align center))
  "Centered text"
  (style '(align left)) nl
  (code "(style '(align center))") nl nl

  (subsection "Code Blocks")
  nl
  "Inline code: " (code "example") " using " (code "(code \"text\")") nl nl

  (subsection "Dividers")
  nl
  "Horizontal line:" nl
  (divider) nl
  (code "(divider)") nl nl

  (subsection "Combining Styles")
  nl
  "Styles can be combined:" nl nl
  (style '(fg "0ff" bg "333" align center))
  (bold "Styled & Centered")
  (reset-style) nl nl

  (code "(style '(fg \"0ff\" bg \"333\" align center))") nl
  (code "(bold \"Styled & Centered\")") nl
  (code "(reset-style)") nl nl

  (subsection "Learn More")
  nl
  (style '(fg "888"))
  "See " (code "framework/micron.scm") " for full implementation."
  (reset-style) nl nl
  (style '(fg "5af"))
  (link "/page/index.mu" "Back to Home")
  (reset-style))
