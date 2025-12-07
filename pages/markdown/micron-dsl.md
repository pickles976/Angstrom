# Micron DSL  

## What is Micron?  

Micron is Nomadnet's markup language for styling terminal pages.
The Micron DSL lets you generate micron markup using Scheme functions.

## Text Styling  

**Bold text** - `(bold "text")`
*Italic text* - `(italic "text")`
Underlined - `(underline "text")`

## Colors  

Set foreground and background colors:

Red text - `(style '(fg "f00"))`
Green background - `(style '(bg "0f0" fg "000"))`

## Headers  

```  

(section "Title") - Top-level section
(subsection "Subtitle") - Sub-section  

```  

## Links  

Create clickable links:

[External Link](https://example.com)
```  

(link "https://example.com" "External Link")  

```  

Internal Page
```  

(file-link "/page/index.mu" "Internal Page")  

```  

## Forms  

Interactive input fields:  

```  

(input-field-fixed "username" 16)  

```  

Submit button:  
```  

(submit-field "Submit" "/path" "page-id" "field1" "field2")  

```  

## Alignment  

```  

(style '(align center))  

```  

## Code Blocks  

Inline code: `example` using `(code "text")`

## Dividers  

Horizontal line:

---

```  

(divider)  

```  

## Combining Styles  

Styles can be combined:

```  

(style '(fg "0ff" bg "333" align center))
(bold "Styled & Centered")
(reset-style)  

```  

## Learn More

See `framework/micron.scm` for full implementation.
