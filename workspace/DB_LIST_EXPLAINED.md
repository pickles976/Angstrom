# db-list: SELECT Queries Explained

## What is db-list?

`db-list` retrieves records from the database, optionally filtering by field values.

## Basic Usage

```scheme
;; Get all comments
(db-list 'comment)

;; Get comments for a specific page
(db-list 'comment '((page-name . "blog_post")))

;; Get Alice's comments on blog_post
(db-list 'comment '((name . "Alice") (page-name . "blog_post")))
```

## Syntax

```scheme
(db-list model-name)
(db-list model-name filters)
```

**Parameters:**
- `model-name` - Symbol identifying the model (e.g., `'comment`, `'post`)
- `filters` - Optional alist of field-value pairs

**Returns:**
- List of instances (alists with `__model__` key)
- Empty list if no matches

## How Filters Work

Filters are an alist where each pair is a field and its value:

```scheme
'((field1 . value1) (field2 . value2))
```

**Examples:**

```scheme
;; Single filter
'((page-name . "welcome"))
; SQL: WHERE page_name = 'welcome'

;; Multiple filters (AND)
'((page-name . "welcome") (name . "Alice"))
; SQL: WHERE page_name = 'welcome' AND name = 'Alice'
```

**Important:** All filters are combined with AND. There's no OR support (yet).

## Return Format

Results are instances - alists with a `__model__` key:

```scheme
((__model__ . comment)
 (id . 7)
 (name . "Bob")
 (address . "")
 (page-name . "welcome")
 (timestamp . "2025-12-04 18:15")
 (text . "Great post!"))
```

## Working with Results

### Accessing Fields

```scheme
(define comments (db-list 'comment '((page-name . "index"))))
(define first-comment (car comments))

(alist-ref 'name first-comment)      ; "Alice"
(alist-ref 'text first-comment)      ; "Hello world"
(alist-ref 'id first-comment)        ; 1
(alist-ref '__model__ first-comment) ; comment
```

### Iterating

```scheme
(for-each
  (lambda (comment)
    (print (alist-ref 'name comment) ": " (alist-ref 'text comment)))
  comments)
```

### Mapping

```scheme
;; Extract all comment texts
(map (lambda (c) (alist-ref 'text c)) comments)
; Returns: ("Hello" "Great!" "Thanks")

;; Build display strings
(map (lambda (c)
       (conc (alist-ref 'name c) " said: " (alist-ref 'text c)))
     comments)
```

### Filtering Results

```scheme
;; Get only comments with IDs > 5
(filter (lambda (c) (> (alist-ref 'id c) 5))
        comments)

;; Get only non-empty addresses
(filter (lambda (c)
          (let ((addr (alist-ref 'address c)))
            (and addr (not (string-null? addr)))))
        comments)
```

## Under the Hood

### Step 1: Build SQL

```scheme
;; No filters
(build-select-sql "comment" '())
; → "SELECT * FROM comment"

;; With filters
(build-select-sql "comment" '((page-name . "welcome")))
; → "SELECT * FROM comment WHERE page_name = ?"
```

### Step 2: Execute Query

```scheme
(define stmt (sql db "SELECT * FROM comment WHERE page_name = ?"))
(define rows (query fetch-all stmt "welcome"))
; rows = ((7 "Bob" "" "welcome" "2025-12-04" "Great!")
;         (8 "Charlie" "abc" "welcome" "2025-12-04" "Thanks!"))
```

### Step 3: Convert Rows to Instances

```scheme
;; Each row is a list of values
(1 "Alice" "" "blog_post" "2025-12-04" "Hello")

;; We have field names from the model
(id name address page-name timestamp text)

;; Zip them together with map cons
(map cons field-names row)
; → ((id . 1) (name . "Alice") (address . "") ...)

;; Add __model__ key
(cons '(__model__ . comment) ...)
; → ((__model__ . comment) (id . 1) (name . "Alice") ...)
```

## Real-World Example: Nomadnet Page

```scheme
(define (display-page-comments page-id)
  "Show all comments for a page"

  (db-open "app.db")

  ;; Query comments for this page
  (define comments (db-list 'comment `((page-name . ,page-id))))

  (if (null? comments)
      (print "No comments yet.")
      (for-each
        (lambda (c)
          (print (alist-ref 'name c) " (" (alist-ref 'timestamp c) ")")
          (print (alist-ref 'text c))
          (print "-"))
        comments))

  (db-close))

;; Use it
(display-page-comments "my-blog-post")
```

## Variadic Functions

`db-list` uses variadic arguments - it accepts optional parameters:

```scheme
(define (db-list model-name . rest)
  ;; 'rest' captures all extra arguments as a list
  (let ((filters (if (null? rest) '() (car rest))))
    ...))
```

**How it works:**

```scheme
(db-list 'comment)
; rest = '()
; filters = '()

(db-list 'comment '((name . "Alice")))
; rest = '(((name . "Alice")))
; filters = '((name . "Alice"))
```

The `.` before `rest` means "collect remaining args into a list."

## Comparison to SQL

| Scheme | SQL |
|--------|-----|
| `(db-list 'comment)` | `SELECT * FROM comment` |
| `(db-list 'comment '((page-name . "index")))` | `SELECT * FROM comment WHERE page_name = 'index'` |
| `(db-list 'comment '((name . "Alice") (page-name . "index")))` | `SELECT * FROM comment WHERE name = 'Alice' AND page_name = 'index'` |

## Safety Features

### SQL Injection Protection

We use parameterized queries:

```scheme
;; SAFE - uses placeholders
(sql db "SELECT * FROM comment WHERE page_name = ?")

;; UNSAFE - don't do this!
(sql db (conc "SELECT * FROM comment WHERE page_name = '" value "'"))
```

### Field Name Sanitization

Hyphens are converted to underscores:

```scheme
'((page-name . "index"))  ; Scheme
; → WHERE page_name = ?   ; SQL
```

## Limitations

1. **AND only** - Can't do OR filters
   ```scheme
   ;; Can't do: name = "Alice" OR name = "Bob"
   ;; Workaround: Query separately and combine
   ```

2. **Equality only** - No <, >, LIKE, etc.
   ```scheme
   ;; Can't do: id > 5
   ;; Workaround: Filter results in Scheme
   (filter (lambda (c) (> (alist-ref 'id c) 5)) all-comments)
   ```

3. **No joins** - Single table only
   ```scheme
   ;; Can't join comments with users
   ;; Workaround: Separate queries
   ```

These are intentional - keeps the ORM simple and learnable!

## Summary

```scheme
;; Query all
(db-list 'comment)

;; Query with filters
(db-list 'comment '((page-name . "index")))

;; Results are alists
(alist-ref 'name (car results))

;; Process with standard list functions
(map ...) (filter ...) (for-each ...)
```

**Key insight:** Results are just data (alists), so you can use all of Scheme's list processing power!
