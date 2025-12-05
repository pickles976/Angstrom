#!/usr/bin/env -S csi -s

;;; markdown-converter.mu - Markdown to Micron Converter Guide

(import micron)
(import markdown)
(load "/home/sebas/.nomadnetwork/storage/pages/app/templates/comments.scm")
(define page-name "markdown-converter")

(print
  (style '(align center))
  (section "Markdown Converter")
  (style '(align left))
  nl nl

  ;; Load and convert markdown content
  (markdown->micron "
## What is the Markdown Converter?

The markdown converter lets you write content in **Markdown** and automatically
convert it to Micron markup at page load time.

This is perfect for:
- Writing documentation quickly
- Focusing on content, not markup
- Using familiar Markdown syntax

## Basic Markdown Syntax

### Headers
```
# H1 Header
## H2 Header
### H3 Header
```

### Emphasis
- **Bold text** with `**bold**` or `__bold__`
- *Italic text* with `*italic*` or `_italic_`

### Lists
Unordered lists use `-`, `*`, or `+`:
```
- Item one
- Item two
  - Nested item
```

Ordered lists use numbers:
```
1. First item
2. Second item
3. Third item
```

### Links
Create links with `[text](url)`:
```
[Nomadnet](https://github.com/markqvist/nomadnet)
```

### Code
Inline code uses backticks: `code here`

Code blocks use triple backticks:
```
def hello():
    print('Hello, World!')
```

## Using in Your Pages

### Load from String
```scheme
(markdown->micron \"# Hello\\n\\nThis is **markdown**.\")
```

### Load from File
```scheme
(markdown-file->micron \"path/to/file.md\")
```

## Example

This entire page section was generated from a markdown string!
The converter handles all the markup translation for you.

## Learn More

Check out `framework/markdown.scm` for the full implementation.
")

  nl nl
  (style '(fg "5af"))
  (link "/page/index.mu" "Back to Home")
  (reset-style)
  (display-comments app-db-path page-id))
