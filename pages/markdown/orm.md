# ORM Guide  

## What is the ORM?  

A simple Object-Relational Mapper for building data-driven Nomadnet apps.
Automatically generates database tables and provides easy CRUD operations.

## Defining Models  

Models are defined as alists in `app/models.scm`:

```  

(define comment-model
  '((name . comment)
    (fields . (
      ((name . id) (type . integer) (primary-key . #t))
      ((name . text) (type . text))))))  

```  

## Generating Tables  

Create database tables from your models:

```  

csi -s framework/manage.scm --generate \
  --db-path /absolute/path/to/app.db \
  --models-path /absolute/path/to/models.scm  

```  

## Using the ORM  

**Initialize:**  

```  

(import orm)
(load "app/settings.scm")
(orm-init (app-models-path))  

```  

**Open/Close Database:**  

```  

(db-open (app-db-path))
(db-close)  

```  

**Create (INSERT):**  

```  

(define comment (make-comment '((text . "Hello"))))
(db-save comment)  

```  

**Read (SELECT):**  

```  

(db-list 'comment)  ; Get all comments
(db-list 'comment '((page-name . "index")))  ; Filter  

```  

## Working with Instances  

Instances are alists with a `__model__` key:  

```  

(define comment (make-comment '((name . "Alice"))))
(alist-ref 'name comment)  ; Get field value  

```  

## Raw SQL  

For complex queries, use `sql-de-lite` directly:  

```  

(let* ((db (open-database path))
       (stmt (sql db "SELECT * FROM comment LIMIT ?"))
       (rows (query fetch-all stmt 10)))
  (close-database db)
  rows)  

```  

See `app/templates/recent-comments.scm` for a complete example.

## Key Concepts  

**Alists:** Association lists for data  
```  

'((name . "Alice") (age . 30))  

```  

**Parameters:** Thread-safe configuration  

```  

(app-db-path) - Get configured database path  

```  

**Auto Constructors:** Generated from models  

```  

(make-comment fields) - Created automatically  

```  

## Limitations  

The ORM is intentionally simple:
* No UPDATE or DELETE
* No JOINs
* Only equality filters (no <, >, LIKE)
* AND filters only (no OR)  

Use raw SQL for complex operations.  

## Learn More  

See `framework/orm-lib.scm` for the full implementation.  
