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

;; 1. Create instances
(define my-comment
  (make-comment
    '((name . "Alice")
      (address . "")
      (page-name . "index")
      (timestamp . "2025-12-04 16:45")
      (text . "Hello world!"))))

;; 2. Open database
(db-open "app.db")

;; 3. Save instances
(db-save my-comment)

;; 4. Close database
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

2. Add a constructor in `orm-lib.scm`:

```scheme
(define (make-user fields)
  "Create a new user instance"
  (make-instance 'user fields))
```

3. Regenerate tables:

```bash
csi -s orm.scm --generate
```

4. Use it:

```scheme
(define new-user
  (make-user
    '((username . "alice")
      (email . "alice@example.com"))))

(db-open "app.db")
(db-save new-user)
(db-close)
```

## Tips

- Field names with hyphens (like `created-at`) are automatically converted to underscores (`created_at`) in SQL
- The `id` field is always auto-generated, don't include it in your instances
- `db-save` returns the ID of the newly inserted row
- Empty strings (`""`) are used for missing optional fields
