# Building Nomadnet Pages with Angstrom

*December 4, 2025*

I've been experimenting with **Angstrom**, a framework for building interactive Nomadnet pages using Chicken Scheme. Here's what I've learned.

## Why Angstrom?

Nomadnet uses *Micron* markup for pages, which is great but can be verbose. Angstrom provides:

- **Scheme DSL** for generating micron
- **Markdown converter** for content
- **Simple ORM** for databases
- **Learning-focused** design

## Getting Started

### Installation

Install Chicken Scheme and the required eggs:

```
sudo chicken-install sql-de-lite srfi-1 srfi-13 srfi-19
```

Then clone Angstrom and build the modules:

```
git clone https://github.com/example/Angstrom
cd Angstrom
csc -s framework/micron.scm
```

### Your First Page

Create a simple page:

```scheme
#!/usr/bin/env -S csi -s

(import micron)

(print
  (section "Hello Nomadnet")
  nl
  "Welcome to my page!"
  nl
  (link "/page/about.mu" "About Me"))
```

## Working with Data

The ORM makes database work easy:

1. Define models in `app/models.scm`
2. Generate tables with `manage.scm`
3. Use simple CRUD operations

Example:

```scheme
(db-open (app-db-path))
(db-save (make-post '((title . "My Post"))))
(db-list 'post)
(db-close)
```

## Markdown Support

This very blog post is written in Markdown! The page loads it with:

```scheme
(markdown-file->micron "posts/Angstrom-intro.md")
```

Much easier than writing raw micron for content-heavy pages.

## What I've Built

Using Angstrom, I've created:

- A comment system for my blog
- A contact form
- A simple wiki
- This blog!

## Next Steps

I'm planning to:

- Add search functionality
- Create more template examples
- Write comprehensive docs

---

**Links**:
- [Angstrom on GitHub](https://github.com/example/Angstrom)
- [Nomadnet Homepage](https://unsigned.io/nomadnet/)
- [My Nomadnet Node](lxmf@myaddress)

*Happy hacking on the mesh!*
