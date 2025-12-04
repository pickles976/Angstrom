# ORM Workshop - Learning Summary

## What We Built

A simple ORM (Object-Relational Mapper) that generates SQLite tables from hand-written model definitions.

## Files Created

1. **models.scm** - Model definitions using association lists
2. **orm.scm** - ORM tool for generating database tables
3. **app.db** - Generated SQLite database

## Usage

```bash
# Generate database tables from models
csi -s orm.scm --generate

# Show help
csi -s orm.scm --help
```

## Key Scheme Concepts Learned

### 1. Association Lists (alists)
Simple key-value pairs in Scheme:
```scheme
'((name . value) (age . 30))
```

Access values with `alist-ref`:
```scheme
(alist-ref 'name my-alist)  ; Returns: value
```

### 2. Quoting with `'`
The quote prevents evaluation, treating code as data:
```scheme
'(1 2 3)        ; A list of numbers, not a function call
(list 1 2 3)    ; Same result, but using the list function
```

### 3. Let Bindings
- `let` - create local variables (can't reference each other)
- `let*` - create local variables (later ones can use earlier ones)

```scheme
(let* ((a 5)
       (b (+ a 1)))  ; b can use a
  (+ a b))           ; Returns: 11
```

### 4. Higher-Order Functions
- `map` - transform each element, returns new list
- `for-each` - perform side effects, returns nothing

```scheme
(map (lambda (x) (* x 2)) '(1 2 3))  ; Returns: (2 4 6)
(for-each print '(1 2 3))            ; Prints each, returns nothing
```

### 5. String Operations
- `symbol->string` - convert symbol to string
- `string-upcase` - uppercase a string (from srfi-13)
- `string-translate` - replace characters
- `string-intersperse` - join strings with separator
- `conc` - concatenate values to string

### 6. Character Literals
```scheme
#\a    ; The character 'a'
#\-    ; The character '-'
#\_    ; The character '_'
```

### 7. Command-Line Arguments
```scheme
(command-line-arguments)  ; Returns list of args
(member "--flag" args)    ; Check if flag present
```

### 8. Working with SQL
```scheme
(open-database "file.db")           ; Open connection
(sql db "SELECT * FROM users")      ; Prepare statement
(exec prepared-statement)           ; Execute
(close-database db)                 ; Close connection
```

## Common Pitfalls We Encountered

### 1. Variable Shadowing
Using `sql` as both a variable name and function name caused errors.

**Bad:**
```scheme
(let ((sql "SELECT..."))
  (exec (sql db sql)))  ; Confusion!
```

**Good:**
```scheme
(let ((sql-statement "SELECT..."))
  (exec (sql db sql-statement)))  ; Clear!
```

### 2. SQL Column Naming
SQLite doesn't allow hyphens in unquoted column names.

**Bad:** `created-at`
**Good:** `created_at`

We solved this with `string-translate` to convert hyphens to underscores.

### 3. Module Imports
Some functions require specific imports:
- `string-intersperse` → `(chicken string)`
- `string-upcase` → `srfi-13`
- `command-line-arguments` → `(chicken process-context)`

## Update: INSERT Operations Complete! ✅

We now have a working API for creating and saving model instances!

### New Files

**orm-lib.scm** - Runtime library with:
- `make-comment` / `make-post` - Constructor functions
- `db-open` / `db-close` - Database connection management
- `db-save` - Save instances to database

**example-usage.scm** - Complete working example

### Usage Example

```scheme
(load "orm-lib.scm")

;; Create an instance
(define my-comment
  (make-comment
    '((name . "Alice")
      (address . "34dba26ea2b9d6ff1b8b55a347f8f083")
      (page-name . "blog_post")
      (timestamp . "2025-12-04 16:45")
      (text . "Great post!"))))

;; Save to database
(db-open "app.db")
(db-save my-comment)
(db-close)
```

### How It Works

**Instance Representation:**
Instances are alists with a special `__model__` key:
```scheme
'((__model__ . comment)
  (name . "Alice")
  (text . "Hello"))
```

**db-save Process:**
1. Extracts model name from instance
2. Loads model definition from models.scm
3. Generates parameterized INSERT SQL
4. Executes with proper value binding
5. Returns the new row ID

**Key Functions:**
- `make-instance` - Creates instance from model name and fields
- `find-model` - Looks up model definition by name
- `instance->insert-sql` - Generates INSERT statement
- `get-instance-values-in-order` - Extracts values in correct order

### New Scheme Concepts Learned

**Parameters** - Thread-safe global state:
```scheme
(define db-connection (make-parameter #f))
(db-connection (open-database "file.db"))  ; Set
(db-connection)                            ; Get
```

**apply** - Spread list as function arguments:
```scheme
(apply exec stmt '("Alice" "text"))
; Same as: (exec stmt "Alice" "text")
```

**find** (from srfi-1) - First matching element:
```scheme
(find (lambda (x) (> x 5)) '(1 3 7 9))  ; Returns: 7
```

**unless** - Inverted if without else:
```scheme
(unless condition
  (do-something))
; Equivalent to: (if (not condition) (do-something))
```

## Update: Automatic Constructor Generation! ✅

Constructors are now **automatically generated** from models.scm!

### How It Works

When you `(load "orm-lib.scm")`, it:
1. Loads models.scm
2. For each model in `all-models`, generates a `make-<model>` function using `eval`
3. Makes them globally available

### Example

Add to models.scm:
```scheme
(define user-model '((name . user) (fields . (...))))
(define all-models (list post-model comment-model user-model))
```

No manual code needed - `make-user` automatically exists!

```scheme
(load "orm-lib.scm")
(make-user '((username . "alice")))  ; Just works!
```

### New Scheme Concepts

**Quasiquote (`)** - Template with interpolation:
```scheme
`(define (,func-name) ...)  ; Like template strings
```

**Eval** - Execute code as data:
```scheme
(eval '(define x 10))       ; Creates variable x
(eval `(define ,name 5))    ; Dynamic definition
```

**Symbol manipulation**:
```scheme
(string->symbol "make-user")  ; Creates symbol 'make-user
(symbol->string 'user)        ; Creates string "user"
```

## Update: SELECT Operations Complete! ✅

We now have `db-list` for querying data with filters!

### New Function: db-list

```scheme
;; Get all records
(db-list 'comment)

;; Filter by field
(db-list 'comment '((page-name . "blog_post")))

;; Multiple filters (AND)
(db-list 'comment '((name . "Alice") (page-name . "index")))
```

### How It Works

**SQL Generation:**
1. Builds `SELECT * FROM table`
2. Adds `WHERE field = ? AND field = ?` if filters provided
3. Uses parameterized queries (safe from SQL injection)

**Row Conversion:**
1. Fetches rows as lists: `(1 "Alice" "" "blog_post" ...)`
2. Gets field names from model: `(id name address page-name ...)`
3. Zips them together: `((id . 1) (name . "Alice") ...)`
4. Adds `__model__` key: `((__model__ . comment) (id . 1) ...)`

**Example:**
```scheme
(db-list 'comment '((page-name . "welcome")))
; Returns:
; (((__model__ . comment) (id . 7) (name . "Bob") (text . "Great!"))
;  ((__model__ . comment) (id . 8) (name . "Charlie") (text . "Thanks!")))
```

### New Scheme Concepts

**Variadic Functions** - Functions accepting variable arguments:
```scheme
(define (db-list model-name . rest)
  ;; rest captures extra arguments as a list
  (let ((filters (if (null? rest) '() (car rest))))
    ...))

;; Can call with or without filters:
(db-list 'comment)                     ; rest = '()
(db-list 'comment '((name . "Alice"))) ; rest = '(((name . "Alice")))
```

**Map with Cons** - Building alists from parallel lists:
```scheme
(map cons '(a b c) '(1 2 3))
; Returns: ((a . 1) (b . 2) (c . 3))
```

## Next Steps

1. ✅ ~~INSERT operations (create records)~~
2. ✅ ~~Automatic constructor generation from models.scm~~
3. ✅ ~~SELECT operations (query records)~~
4. (Optional) A `define-model` macro for cleaner syntax
5. (Optional) Helper functions like `db-find-by-id`
