# Claude Onboarding Guide - Nomadnet ORM

## What This Is

A **simple, learnable ORM** for building Nomadnet applications in **Chicken Scheme**. Nomadnet is a mesh communication platform; it uses **micron** (a terminal-friendly markup language) to render pages.

**Philosophy**: Learning-focused. Keep it simple. Every feature should teach Scheme concepts.

## Project Status: Fully Functional

- ✅ Table generation from models
- ✅ Auto-generated constructors (via eval/quasiquote)
- ✅ INSERT (db-save)
- ✅ SELECT with filtering (db-list)
- ✅ Working Nomadnet page example
- ✅ **Complete micron-dsl** with full feature set (forms, colors, alignment, etc.)
- ⚠️ No UPDATE/DELETE (intentionally simple - user doesn't need them)

## Directory Structure

```
workspace/
├── src/
│   ├── orm.scm          # CLI: csi -s orm.scm --generate
│   ├── orm-lib.scm      # Runtime library (load this in user code)
│   ├── models.scm       # Model definitions (alist format)
│   └── micron-dsl.scm   # DSL for generating micron markup
│
├── pages/               # Working Nomadnet page example
│   ├── index.mu         # Main page (run from workspace root)
│   └── app/
│       ├── actions/handle_comment.scm
│       └── templates/comments.scm
│
├── docs/                # Learning materials & runnable examples
│   ├── README.md        # Complete learning guide
│   ├── QUICK_START.md   # API reference
│   └── *.scm           # Runnable examples
│
├── app.db              # SQLite database
└── README.md           # User-facing intro
```

## Critical Path Knowledge

### 0. IMPORTANT: Framework Module Imports

**NEVER use relative imports for framework modules (`micron`, `markdown`, `orm`).**

```scheme
;; ✅ CORRECT - Always use import
(import micron)
(import markdown)
(import orm)

;; ❌ WRONG - Never load framework modules relatively
(load "../framework/micron.scm")
(load "../framework/markdown.scm")
```

**Why?**
- Framework modules are compiled and installed system-wide
- They export specific functions through module definitions
- Relative loads bypass the module system and can cause issues

**To update framework modules:**
```bash
cd framework
sudo chicken-uninstall micron    # Remove old version
sudo chicken-uninstall markdown
sudo chicken-uninstall orm
sudo csc -s micron.scm -J && sudo chicken-install
sudo csc -s markdown.scm -J && sudo chicken-install
sudo csc -s orm-lib.scm -J && sudo chicken-install
```

**User code in pages/app/** can use relative loads for local files, but framework modules must always be imported.

### 1. All Paths Are Relative to Workspace Root

**Run everything from `/workspace/`**:
```bash
cd workspace
csi -s pages/index.mu        # Works
csi -s src/orm.scm --generate # Works
csi -s docs/example.scm      # Needs path fixes (see note below)
```

**Important**: `src/orm-lib.scm` loads `src/models.scm` (hardcoded path). All user scripts should run from workspace root.

### 2. Model Format (Alist-Based)

```scheme
(define comment-model
  '((name . comment)              ; Table name
    (fields . (                   ; List of field definitions
      ((name . id)
       (type . integer)
       (primary-key . #t)
       (autoincrement . #t))
      ((name . page-name)         ; Hyphens auto-convert to underscores in SQL
       (type . text)
       (size . 32))))))

(define all-models (list comment-model post-model user-model))
```

**Add a model → get `make-<model>` constructor automatically** (via eval/quasiquote magic).

### 3. Core API Pattern

```scheme
(load "src/orm-lib.scm")  ; Auto-generates constructors from models

(db-open "app.db")

;; CREATE
(db-save (make-comment '((name . "Alice") (text . "Hi"))))

;; READ
(db-list 'comment)                              ; All
(db-list 'comment '((page-name . "index")))     ; Filtered

(db-close)
```

### 4. Instance Format

Everything is **alists** with a special `__model__` key:

```scheme
'((__model__ . comment)
  (id . 1)
  (name . "Alice")
  (text . "Hello"))
```

Access with: `(alist-ref 'name instance)`

### 5. Key Design Patterns

**Parameters (not globals)**:
```scheme
(define db-connection (make-parameter #f))
(db-connection)         ; GET
(db-connection value)   ; SET
```
Thread-safe, cleaner than `set!`.

**Automatic Constructors**:
```scheme
(eval `(define (,constructor-name fields)
         (make-instance ',model-name fields)))
```
On load, generates `make-comment`, `make-post`, etc. from models.

**Variadic Functions**:
```scheme
(define (db-list model-name . rest)
  (let ((filters (if (null? rest) '() (car rest))))
    ...))
```
Filters are optional.

## Important Gotchas

### Hyphen → Underscore Conversion

Field names with hyphens (`page-name`) become underscores in SQL (`page_name`):
```scheme
(string-translate (symbol->string 'page-name) #\- #\_)
```

### Variable Shadowing

Don't name local vars `sql` (conflicts with sql-de-lite function):
```scheme
(let ((sql-statement ...))  ; Good
  (sql db sql-statement))

(let ((sql ...))            ; Bad - shadows sql function
  (sql db sql))
```

### The `id` Field

- Always auto-generated (never include in CREATE)
- Always included in SELECT results
- Use `model-field-names` (excludes id) for INSERT
- Use `model-all-field-names` (includes id) for SELECT

## Chicken Scheme Egg Dependencies

```bash
sudo chicken-install sql-de-lite srfi-1 srfi-13 srfi-19
```

## Common Tasks

### Add a New Model

1. Edit `src/models.scm`:
   ```scheme
   (define new-model '((name . new) (fields . (...))))
   (define all-models (list ... new-model))
   ```

2. Regenerate tables:
   ```bash
   csi -s src/orm.scm --generate
   ```

3. Use it - constructor auto-exists:
   ```scheme
   (make-new '((field . value)))
   ```

### Debug Database

```bash
sqlite3 app.db
.schema
SELECT * FROM comment;
```

### Run Examples

Most examples in `docs/` need path updates:
```scheme
(load "src/orm-lib.scm")  # Not "orm-lib.scm"
```

Or run from workspace root after fixing paths.

## Teaching Focus Areas

This project teaches:
- **Alists** - simplest data structure
- **Parameters** - better than globals
- **Quasiquote/eval** - metaprogramming
- **Variadic functions** - `(fn arg . rest)`
- **Map/filter/for-each** - functional lists
- **SQL integration** - parameterized queries

## Known Limitations (By Design)

- **No UPDATE/DELETE** - keeps it simple
- **No OR in filters** - only AND (workaround: filter in Scheme)
- **No joins** - single table queries only
- **Equality only** - no <, >, LIKE (workaround: filter results)

These are intentional - this is a learning ORM, not production.

## User's Learning Style

- Wants **iterative building** - explain each step
- Prefers **simple language features** over clever tricks
- Values **inline documentation** and examples
- Likes seeing **practical applications** (the Nomadnet page)

## Next Session Priorities

If user continues:
1. ✅ Core ORM is complete
2. Possible additions:
   - Helper: `(db-find-by-id 'comment 5)`
   - Helper: `(db-first 'comment filters)`
   - `define-model` macro (cleaner syntax)
   - More Nomadnet page examples

## Quick Reference

| Task | Command |
|------|---------|
| Generate tables | `csi -s src/orm.scm --generate` |
| Test page | `csi -s pages/index.mu` |
| Run example | `csi -s docs/full-crud-demo.scm` |
| Check DB | `sqlite3 app.db` |

## Files to Read First

1. `src/models.scm` - See model format
2. `src/orm-lib.scm:40-85` - Constructor generation (the magic)
3. `src/orm-lib.scm:160-170` - db-connection parameter
4. `src/orm-lib.scm:241-274` - db-list implementation
5. `src/micron-dsl.scm` - **Complete micron DSL** (264 lines, all features)
6. `pages/index.mu` - See it all in action
7. `docs/MICRON_DSL_REFERENCE.md` - Micron DSL quick reference

## Debugging Tips

- **Load errors**: Check paths are relative to workspace root
- **"unbound variable"**: Missing import (check srfi-1, srfi-13)
- **SQL errors**: Check field name hyphen conversion
- **"cannot open file"**: Wrong working directory (should be in workspace/)

## User Context

- Learning Scheme for first time
- Building mesh network apps with Nomadnet
- Previously had file-based comments, now using SQLite
- Appreciates detailed explanations and learning resources

---

**TL;DR**: Simple ORM teaching Scheme through practical use. Everything runs from workspace root. Models are alists. Instances are alists. Constructors auto-generate. It all just works.
