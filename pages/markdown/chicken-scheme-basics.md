# Chicken Scheme Basics

## What is Scheme?  

Scheme is a minimalist Lisp dialect that emphasizes simplicity and elegance.
Chicken Scheme compiles to C, making it fast and portable.

## Basic Syntax  

Everything is an expression in parentheses:

```    

(+ 1 2)  ; Returns 3
(string-append "Hello" " " "World")  ; Returns "Hello World"
(if (> 5 3) "yes" "no")  ; Returns "yes"  

```  

## Defining Variables  

```  

(define name "Alice")
(define age 30)
(define greeting (string-append "Hello, " name))  

```  

## Functions  

Define functions with `define`:

```  

(define (greet name)
  (conc "Hello, " name "!"))

(greet "Bob")  ; Returns "Hello, Bob!"  

```  

## Lists  

Lists are fundamental in Scheme:

```  

(list 1 2 3)  ; Create a list
(car '(1 2 3))  ; Get first element (1)
(cdr '(1 2 3))  ; Get rest of list (2 3)
(cons 0 '(1 2 3))  ; Add to front (0 1 2 3)  

```  

## Association Lists (Alists)  

Key-value pairs used throughout Angstrom:

```  

(define person '((name . "Alice") (age . 30)))
(alist-ref 'name person)  ; Returns "Alice"  

```  

## Common Functions  

**Map:** Apply function to each element
```  

(map (lambda (x) (* x 2)) '(1 2 3))  ; Returns (2 4 6)  

```  

**Filter:** Keep elements matching predicate  

```  

(filter odd? '(1 2 3 4))  ; Returns (1 3)  

```  

**For-each:** Side effects on each element  

```  

(for-each print '("a" "b" "c"))  ; Prints a, b, c  

```  


## Let Bindings  

Create local variables:

```  

(let ((x 10)
      (y 20))
  (+ x y))  ; Returns 30  

```  

## Learn More  

[Chicken Scheme Manual](https://www.call-cc.org/manual/)
