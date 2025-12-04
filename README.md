# README

This is a work in progress library for generating micron pages for [nomadnet](https://github.com/markqvist/NomadNet) in [chicken scheme](https://call-cc.org/).

![](./images/micron_generator_demo.png)

## Project Layout

```bash
pages
|____micron-dsl.scm
|
|____index.mu
|
|____app
    |_____models.scm // SQLite functions go here
    |
    |_____actions
    |     |________handle_comments.mu // Each file does CRUD + other stuff
    |
    |_____templates
          |________comments.mu // template functions
```

## Run Locally

Install chicken scheme
```bash
sudo apt update
sudo apt-get install chicken-bin
sudo chicken-install sql-de-lite
sudo chicken-install srfi-1
sudo chicken-install srfi-13
sudo chicken-install srfi-19
```

Run the demo page manually:
```bash
cd ./framework/pages
csi -s ./index.mu 
```

Or just copy the contents of `/framework/pages` to `~/.nomadnetwork/storage/pages`

### Tips

```
(db-list (comment 'page-index "blog_post")) ;; returns all comments for that page
```

#### Learning Scheme

I chose Scheme for a few reasons.
1. I saw someone else use Chicken Scheme for making a micron dsl
2. I have wanted to have a project to learn Scheme
3. Scheme's syntax of S-expressions is much easier to read and reason about for site generation than something like Python. Think about how readable
html or jsx is for creating UI. The micron-dsl is similar, except with less nesting due to the non-branching nature of Micron.
4. Chicken Scheme can compile to C, which means that complicated applications can run on small boards like the Pi Zero. 

However, I realize that most people are probably not familiar with Scheme's syntax. If your goal is just to get a simple site up and running, I recommend just following a few Scheme tutorials to learn the absolute basics of the Syntax, and then copying one of the examples and modifying it to your liking. 

If you would like to implement new or complex functionality. I would recommend the following book on Scheme: [The Schematics of Computation by Manis and Little](https://www.math.purdue.edu/~lucier/schematics-front.pdf). I found it to be much more useful than SICP or "The Little Schemer", which are the books that people usually recommend. The Racket book [How to Design Programs](https://htdp.org/) is also a really good (and free) introduction to Scheme/Lisp languages.

I am a Scheme novice myself, so this codebase will remain simple since I couldn't make it more complicated even if I wanted to.

#### Developer Experience

If using vscode, download the Scheme extension and make sure that .mu files are recognized as scheme:
`Preferences -> Settings -> type: "files.associations"` and add a mapping from `*.mu` to `scheme`.

Install a scheme language server. 
```bash
sudo chicken-install -s apropos chicken-doc srfi-18
cd `csi -R chicken.platform -p '(chicken-home)'`
curl http://3e8.org/pub/chicken-doc/chicken-doc-repo.tgz | sudo tar zx
sudo chicken-install lsp-server
```

Add a scheme lsp extension to vscode. Open the command palette and type `ext install rgherdt.scheme-lsp`

### TODO



- [ ] see if claude can create a simple ORM
    - [x] models.scm file
    - [x] orm --generate (makes file and tables)
    - [x] insert command
    - [x] return a list (filter by specific keywords)
- [ ] create documentation

- [ ] implement the rest of micron-dsl
- [ ] load and convert markdown content into 
- [ ] separate all the examples out into "docs" or "examples"
- [ ] compile the custom modules
- [ ] usage
    - [ ] multiple pages generated with micron-dsl.scm 
        - [ ] index (link to repo)
        - [ ] blog post 0 (explain the basics of chicken scheme)
        - [ ] blog post 1 (explain how the DSL works)
        - [ ] blog post 2 (explain the markdown converter)
        - [ ] blog post 3 (explain how the ORM works)
    - [ ] orm.scm
        - [ ] add comment sections to the bottom of each page
- [ ] get it working in-situ

example app
- [ ] get lsp working

- [ ] add instructions for users to build and use the custom modules
- [ ] add some tutorials

https://wiki.call-cc.org/chicken-for-python-programmers

- [ ] integrate recipe search
    - [ ] run hari.recipes locally
    - [ ] query the API and html
    - [ ] parse and list
    - [ ] click on list entry and load HTML
    - [ ] convert HTML to micron
- [ ] create epub search
- [ ] update with learning