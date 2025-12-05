# Auto-Generated Constructors

The ORM automatically generates `make-<model>` functions for each model. Here's how it works.

## The Magic

When you call `(orm-init "models.scm")`, the ORM:

1. Loads your models from the file
2. For each model, generates a constructor function
3. Makes these constructors available globally

```scheme
;; In models.scm
(define comment-model '((name . comment) (fields . (...))))

;; After (orm-init "models.scm")
;; You can now use:
(make-comment '((text . "Hello")))  ; ← This was auto-generated!
```

## How It Works

The ORM uses quasiquote and eval to generate functions at runtime:

```scheme
;; Simplified version of what the ORM does
(define (generate-constructor model)
  (let ((model-name (alist-ref 'name model)))
    ;; Build function name: make-comment
    (let ((fn-name (string->symbol
                     (string-append "make-" (symbol->string model-name)))))

      ;; Evaluate code that defines the function
      (eval `(define (,fn-name fields)
               (make-instance ',model-name fields))))))
```

## What's Happening?

### Step 1: Quasiquote Template

```scheme
`(define (,fn-name fields)
   (make-instance ',model-name fields))
```

The backtick (`) creates a template. The commas (,) insert values.

### Step 2: Template Expands

For `comment-model`, this becomes:

```scheme
(define (make-comment fields)
  (make-instance 'comment fields))
```

### Step 3: Eval Executes

`eval` runs this code, creating the function in the global environment.

## Why This Approach?

**Pros:**
- Clean API: `(make-comment ...)` is nicer than `(make-instance 'comment ...)`
- Type-safe-ish: Typos caught earlier (`make-commet` → error)
- Extensible: Easy to add more auto-generation

**Cons:**
- Uses `eval` (generally discouraged in production)
- Dynamic: IDEs can't autocomplete these functions
- Educational: Designed for learning, not production

## Alternative Approaches

### Manual Constructors (Simple)

```scheme
(define (make-comment fields)
  (make-instance 'comment fields))

(define (make-post fields)
  (make-instance 'post fields))
```

**Pros**: Simple, explicit
**Cons**: Repetitive, easy to forget

### Macro-Based (Advanced)

```scheme
(define-syntax define-model
  (syntax-rules ()
    ((_ name fields)
     (begin
       (define name-model '((name . name) (fields . fields)))
       (define (make-name fields)
         (make-instance 'name fields))))))

(define-model comment (...))
```

**Pros**: Compile-time, IDE-friendly
**Cons**: More complex, harder to learn

## The Learning Journey

Angstrom uses eval-based generation because it's:
1. Easy to understand for beginners
2. Shows how metaprogramming works
3. Demonstrates quasiquote/eval patterns

For production, consider using macros or manual definitions.

## See It in Action

Check out:
- `framework/orm-lib.scm:75-95` - The actual implementation
- `docs/orm/01-basic-usage.scm` - Using generated constructors
