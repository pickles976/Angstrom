# Understanding Parameters

Parameters are Scheme's thread-safe way to handle configuration values.

## What are Parameters?

A parameter is a special procedure that acts like a getter/setter:

```scheme
(define my-config (make-parameter "default"))

(my-config)           ; GET - returns "default"
(my-config "new")     ; SET - changes to "new"
(my-config)           ; GET - now returns "new"
```

## Why Use Parameters?

1. **Thread-safe**: Unlike global variables, parameters are thread-local
2. **Cleaner**: No `set!` mutation
3. **Testable**: Easy to temporarily override in tests

## Parameters in Angstrom

### Database Path

```scheme
;; In settings.scm
(define db-path "/absolute/path/to/app.db")
(define app-db-path (make-parameter db-path))

;; In your code
(db-open (app-db-path))  ; Use the parameter
```

### Models Path

```scheme
;; In settings.scm
(define models-path "/absolute/path/to/models.scm")
(define app-models-path (make-parameter models-path))

;; In your code
(orm-init (app-models-path))  ; Use the parameter
```

## How the ORM Uses Parameters

The ORM module uses an internal parameter for loaded models:

```scheme
;; In framework/orm-lib.scm
(define all-models (make-parameter '()))

;; When you call orm-init:
(orm-init "models.scm")           ; Loads models
(all-models (eval 'all-models))   ; Stores in parameter
```

This allows the ORM to:
- Store models in a thread-safe way
- Avoid global mutation
- Support multiple model sets (if needed)

## Comparison: Parameters vs Globals

```scheme
;; ❌ Old way: Global variable
(define db-path "app.db")
(set! db-path "new.db")  ; Mutation, not thread-safe

;; ✅ Modern way: Parameter
(define app-db-path (make-parameter "app.db"))
(app-db-path "new.db")   ; Clean, thread-safe
```

## Learn More

Parameters are part of SRFI-39. See:
- [SRFI-39 Specification](https://srfi.schemers.org/srfi-39/)
- Chicken Scheme manual on parameters
