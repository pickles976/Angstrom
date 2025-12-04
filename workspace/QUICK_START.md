# ORM Quick Start Guide

## Setup

```bash
cd workspace

# 1. Generate database tables from models
csi -s orm.scm --generate

# 2. Run the example
csi -s example-usage.scm

# 3. Check the data
sqlite3 app.db "SELECT * FROM comment;"
sqlite3 app.db "SELECT * FROM post;"
```

## Usage Pattern

```scheme
#!/usr/bin/env -S csi -s

(load "orm-lib.scm")

;; Open database
(db-open "app.db")

;; CREATE: Make and save instances
(define my-comment
  (make-comment
    '((name . "Alice")
      (address . "")
      (page-name . "index")
      (timestamp . "2025-12-04 16:45")
      (text . "Hello world!"))))
(db-save my-comment)

;; READ: Query instances
(define all-comments (db-list 'comment))
(define index-comments (db-list 'comment '((page-name . "index"))))
(define alice-index (db-list 'comment '((name . "Alice") (page-name . "index"))))

;; Work with results (they're just alists!)
(for-each
  (lambda (c)
    (print (alist-ref 'name c) ": " (alist-ref 'text c)))
  index-comments)

;; Close database
(db-close)
```

## File Structure

```
workspace/
├── models.scm          # Model definitions (edit this)
├── orm.scm             # CLI tool for generating tables
├── orm-lib.scm         # Runtime library (load in your scripts)
├── example-usage.scm   # Working example
├── app.db              # Generated SQLite database
└── README.md           # Learning documentation
```

## Adding a New Model

**Now fully automated!** Just add to models.scm - no manual constructor code needed.

1. Edit `models.scm` and add your model to `all-models`:

```scheme
(define user-model
  '((name . user)
    (fields . (
      ((name . id)
       (type . integer)
       (primary-key . #t)
       (autoincrement . #t))
      ((name . username)
       (type . text)
       (size . 32))
      ((name . email)
       (type . text))))))

(define all-models
  (list post-model comment-model user-model))  ; Add here
```

2. Regenerate tables:

```bash
csi -s orm.scm --generate
```

3. Use it! (make-user is automatically generated):

```scheme
(define new-user
  (make-user
    '((username . "alice")
      (email . "alice@example.com"))))

(db-open "app.db")
(db-save new-user)
(db-close)
```

## API Reference

### Creating Data

```scheme
(db-save (make-comment '((name . "Alice") (text . "Hello"))))
```

### Querying Data

```scheme
;; Get all records
(db-list 'comment)

;; Filter by one field
(db-list 'comment '((page-name . "index")))

;; Filter by multiple fields (AND)
(db-list 'comment '((page-name . "index") (name . "Alice")))
```

### Working with Results

Results are alists with a `__model__` key:

```scheme
(define comments (db-list 'comment '((page-name . "index"))))

;; Access fields
(alist-ref 'name (car comments))
(alist-ref 'text (car comments))
(alist-ref 'id (car comments))

;; Map over results
(map (lambda (c) (alist-ref 'text c)) comments)

;; Filter results
(filter (lambda (c) (> (alist-ref 'id c) 5)) comments)
```

## Tips

- Field names with hyphens (like `created-at`) are automatically converted to underscores (`created_at`) in SQL
- The `id` field is always auto-generated, don't include it when creating instances
- The `id` field IS included in query results
- `db-save` returns the ID of the newly inserted row
- `db-list` returns an empty list if no results match
- Empty strings (`""`) are used for missing optional fields
