# Micron DSL Documentation

Generate Micron markup using Scheme functions instead of writing raw markup strings.

## What is Micron?

Micron is Nomadnet's markup language for terminal-based pages. It supports:
- Text styling (bold, italic, colors)
- Links and navigation
- Interactive forms
- Layout control

## What is the Micron DSL?

The DSL (Domain-Specific Language) provides Scheme functions that generate micron markup:

```scheme
;; Instead of writing:
"`!Bold`! and `*italic`*"

;; You write:
(conc (bold "Bold") " and " (italic "italic"))
```

## Quick Start

```scheme
(import micron)

(print
  (section "Welcome")
  nl
  (style '(fg "5af"))
  "This text is light blue"
  (reset-style)
  nl
  (link "https://example.com" "Click here"))
```

## Core Functions

### Text Styling

```scheme
(bold "text")       ; `!text`!
(italics "text")    ; `*text`*
(underline "text")  ; `_text`_
(code "text")       ; `=text`=
```

### Colors & Styles

```scheme
(style '(fg "f00"))                ; Red foreground
(style '(bg "0f0"))                ; Green background
(style '(fg "fff" bg "000"))       ; White on black
(style '(align center))            ; Center alignment
(reset-style)                      ; Reset all styles
```

Color format: 3-digit hex (RGB)
- `"f00"` = red
- `"0f0"` = green
- `"5af"` = light blue

### Headers

```scheme
(section "Title")       ; > Title
(subsection "Subtitle") ; >> Subtitle
```

### Links

```scheme
(link "https://example.com" "External Link")
(link "/page/other.mu" "Internal Page")
(link "lxmf@address" "LXMF Contact")
```

### Forms

```scheme
;; Input field
(input-field-fixed "fieldname" 32)

;; Submit button
(submit-field "Submit" "/action.scm" "page-id" "field1" "field2")
```

### Layout

```scheme
nl                  ; Newline
(divider)           ; Horizontal line
(style '(align left|center|right))
```

## Combining Everything

```scheme
(print
  (style '(align center))
  (section "My Page")
  (style '(align left))
  nl nl

  (style '(fg "ddd"))
  "Welcome to " (bold "Angstrom") "!"
  (reset-style)
  nl nl

  (style '(fg "5af"))
  (link "/page/docs.mu" "Read the docs")
  (reset-style))
```

## Files in This Folder

- `demo.scm` - Complete demonstration of all features
- `forms-example.scm` - Interactive form building
- `styling-guide.md` - Advanced styling techniques
