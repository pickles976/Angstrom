# How `db-connection` Works

## The Code

```scheme
(define db-connection (make-parameter #f))

(define (db-open filename)
  (db-connection (open-database filename)))

(define (db-close)
  (when (db-connection)
    (close-database (db-connection))
    (db-connection #f)))

(define (db-save instance)
  (unless (db-connection)
    (error "No database connection!"))
  ...
  (apply exec (sql (db-connection) sql-statement) values)
  ...
  (let ((id (last-insert-rowid (db-connection))))
    id))
```

## What is a Parameter?

A **parameter** is Chicken Scheme's way of managing global state safely. Think of it as a smart global variable.

### Creating a Parameter

```scheme
(define db-connection (make-parameter #f))
```

This creates a parameter called `db-connection` with initial value `#f` (false).

### Using Parameters: Two Modes

Parameters act like a **dual-mode function**:

#### Mode 1: GET (no arguments)
```scheme
(db-connection)  ; Returns current value
```

#### Mode 2: SET (one argument)
```scheme
(db-connection new-value)  ; Sets value to new-value
```

## Flow Diagram

```
User Code                    Parameter State
─────────                    ───────────────

(db-open "app.db")
    ↓
db-connection(             db-connection = #f
  open-database("app.db")      ↓
)                          db-connection = <SQLite handle>
    ↓
(db-save my-comment)
    ↓
(db-connection)            ; GET - returns <SQLite handle>
    ↓
Use the handle for SQL
    ↓
(db-close)
    ↓
db-connection(#f)          db-connection = #f
```

## Detailed Walkthrough

### Step 1: Initial State

```scheme
(define db-connection (make-parameter #f))
```

- Creates parameter
- Initial value: `#f` (not connected)
- `(db-connection)` → `#f`

### Step 2: Opening Database

```scheme
(db-open "app.db")
  ↓
(db-connection (open-database filename))
```

**What happens:**
1. `(open-database "app.db")` returns a SQLite database handle
2. `(db-connection <handle>)` **sets** the parameter to that handle
3. Now `(db-connection)` **gets** the handle

### Step 3: Using Database

```scheme
(db-save instance)
  ↓
(unless (db-connection)  ; GET: check if we have a connection
  (error "No connection!"))
  ↓
(sql (db-connection) sql-statement)  ; GET: use the connection
  ↓
(last-insert-rowid (db-connection))  ; GET: use the connection again
```

**What happens:**
- Every call to `(db-connection)` with **no arguments** returns the current handle
- We use this handle to execute SQL commands

### Step 4: Closing Database

```scheme
(db-close)
  ↓
(when (db-connection)              ; GET: check if connected
  (close-database (db-connection)) ; GET: get handle to close
  (db-connection #f))              ; SET: reset to #f
```

**What happens:**
1. Check if connected: `(db-connection)` → handle or `#f`
2. If connected, close it: `(close-database handle)`
3. Reset parameter: `(db-connection #f)`

## Why Not Just Use a Variable?

### With Regular Variable (messy)

```scheme
(define db #f)

(define (db-open filename)
  (set! db (open-database filename)))

(define (db-close)
  (when db
    (close-database db)
    (set! db #f)))

(define (db-save instance)
  (if (not db)
      (error "No connection!")))
```

**Problems:**
- Need `set!` everywhere (mutation)
- Not thread-safe
- No dynamic scoping

### With Parameter (clean)

```scheme
(define db-connection (make-parameter #f))

(define (db-open filename)
  (db-connection (open-database filename)))

(define (db-close)
  (when (db-connection)
    (close-database (db-connection))
    (db-connection #f)))
```

**Benefits:**
- Cleaner syntax: `(db-connection value)` vs `(set! db value)`
- Thread-safe automatically
- Self-documenting: `(db-connection)` clearly means "get current connection"
- Supports `parameterize` for temporary changes

## Advanced: Dynamic Scoping

Parameters support **dynamic scoping** with `parameterize`:

```scheme
(define db-connection (make-parameter #f))

;; Open main database
(db-connection (open-database "main.db"))

(print (query-users))  ; Uses main.db

;; Temporarily use test database
(parameterize ((db-connection (open-database "test.db")))
  (print (query-users))  ; Uses test.db
  (db-save some-data))   ; Saves to test.db

;; Back to main database
(print (query-users))  ; Uses main.db again
```

This is incredibly useful for testing!

## Real-World Analogy

Think of a parameter like a **receptionist's desk**:

```
Regular variable:
  You write on a sticky note and stick it to the desk.
  Anyone can grab it, change it, throw it away.

Parameter:
  You have a special tray on the desk.
  - PUT something in: (param value)
  - LOOK in the tray: (param)
  - The tray is managed safely, thread-aware
```

## Common Pattern: Checking If Connected

```scheme
;; Check and error if not connected
(unless (db-connection)
  (error "No database connection!"))

;; Check and use if connected
(when (db-connection)
  (do-something-with (db-connection)))

;; Safe get with default
(or (db-connection) default-db)
```

## Summary

**Parameters are procedures that act as smart containers:**

| Call | Meaning | Example |
|------|---------|---------|
| `(make-parameter val)` | Create with initial value | `(make-parameter #f)` |
| `(param)` | GET current value | `(db-connection)` |
| `(param val)` | SET to new value | `(db-connection db)` |
| `(when (param) ...)` | Check if truthy | Check if connected |
| `(parameterize ...)` | Temporary override | Use test database |

**In our ORM:**
- `db-connection` holds the SQLite database handle
- `db-open` sets it
- `db-save`, `db-close` read it
- All functions share the same connection automatically
