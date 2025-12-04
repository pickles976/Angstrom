# Parameters: Quick Reference

## The Magic Syntax

```scheme
(define db-connection (make-parameter #f))
```

Creates a **parameter** - a special function that acts like a smart variable.

## Two Modes of Operation

### GET Mode (no arguments)
```scheme
(db-connection)          ; Returns current value
```

### SET Mode (one argument)
```scheme
(db-connection value)    ; Sets to new value
```

## In Our ORM

### Setup (orm-lib.scm:160)
```scheme
(define db-connection (make-parameter #f))
```
Creates parameter, initially `#f` (no connection)

### Opening Database (orm-lib.scm:162-164)
```scheme
(define (db-open filename)
  (db-connection (open-database filename)))
```
- `(open-database filename)` returns SQLite handle
- `(db-connection handle)` **sets** parameter to that handle

### Using Database (orm-lib.scm:179-195)
```scheme
(define (db-save instance)
  (unless (db-connection)           ; GET: check if connected
    (error "No connection!"))

  ...
  (apply exec
    (sql (db-connection) stmt)      ; GET: use connection
    values)

  (last-insert-rowid (db-connection)))  ; GET: use again
```
Every `(db-connection)` with no args **gets** the current handle

### Closing Database (orm-lib.scm:166-170)
```scheme
(define (db-close)
  (when (db-connection)              ; GET: check
    (close-database (db-connection)) ; GET: get handle
    (db-connection #f)))             ; SET: reset
```

## Why It's Brilliant

### Before (with set!)
```scheme
(define db #f)
(set! db value)    ; Need special mutation form
(if db ...)        ; Check value
```

### After (with parameters)
```scheme
(define db-connection (make-parameter #f))
(db-connection value)   ; Cleaner set
(if (db-connection) ...)   ; Cleaner get
```

## The Pattern

**Parameters follow a consistent pattern:**

1. **Create once** at top level:
   ```scheme
   (define my-param (make-parameter initial-value))
   ```

2. **Set when needed**:
   ```scheme
   (my-param new-value)
   ```

3. **Get whenever used**:
   ```scheme
   (my-param)  ; Returns current value
   ```

## Examples from ORM

| Code | What it does |
|------|--------------|
| `(make-parameter #f)` | Create parameter, initially false |
| `(db-connection)` | Get current connection |
| `(db-connection db)` | Set connection to db |
| `(when (db-connection) ...)` | If connected, do something |
| `(unless (db-connection) ...)` | If NOT connected, error |

## Try It Yourself

```bash
cd workspace
csi -s parameter-interactive.scm   # Simple demo
csi -s parameters-explained.scm    # Detailed examples
```

## The Mental Model

Think of `db-connection` as a **box**:

```
(make-parameter #f)      Create empty box
(db-connection)          Look inside box
(db-connection value)    Put something in box
```

The box is **globally accessible** but **safely managed**.

## Thread Safety

Unlike regular variables, parameters are thread-safe:

```scheme
;; Regular variable - NOT thread-safe
(define db #f)
(set! db value)

;; Parameter - IS thread-safe
(define db-connection (make-parameter #f))
(db-connection value)
```

Different threads can have different parameter values!

## Related Functions

```scheme
;; Check truthiness
(if (db-connection) ...)
(when (db-connection) ...)
(unless (db-connection) ...)

;; Get with default
(or (db-connection) default-db)

;; Temporary override
(parameterize ((db-connection temp-db))
  ...)  ; Only temp-db here
```

## Bottom Line

**`db-connection` is a function that:**
- With 0 args → returns stored value (GET)
- With 1 arg → stores new value (SET)
- Thread-safe and clean to use
- Avoids messy `set!` mutations
