# Macron

Tools for building Nomadnet apps in Chicken Scheme. Includes an ORM, a micron DSL, and a markdown converter.

## Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/pickles976/Macro.git
   cd Macro
   ```

2. **Install Chicken Scheme and dependencies**
   ```bash
   sudo apt-get install chicken-bin
   sudo chicken-install sql-de-lite srfi-1 srfi-13 srfi-19
   ```

3. **Build and install Macron modules**
   ```bash
   cd ./framework

   # Build the modules
   csc -s micron.scm
   csc -s markdown.scm
   csc -s orm-lib.scm

   # Install system-wide (optional)
   sudo chicken-install -s micron.egg
   sudo chicken-install -s markdown.egg
   sudo chicken-install -s orm.egg
   ```

   Then use them anywhere:
   ```scheme
   (import micron)
   (import markdown)
   (import orm)
   ```

## Building a Project

Copy the contents of `pages` to `~/.nomadnetwork/storage`
Run `chmod +x` on any pages or sub-pages that will be run as scripts.

### If Using the ORM

Generate the sqlite file from your `models.scm` file:
```bash
csi -s framework/orm.scm --generate <path-to-models-file>
csi -s pages/index.mu
```

Edit `settings.scm` so that `db-path` and `models-path` point to your `models.scm` file and your desire sqlite path location.

## Learning Scheme

I chose Scheme for a few reasons.
1. I saw someone else use Chicken Scheme for making a micron dsl
2. I have wanted to have a project to learn Scheme
3. Scheme's syntax of S-expressions is much easier to read and reason about for site generation than something like Python. Think about how readable
html or jsx is for creating UI. The micron-dsl is similar, except with less nesting due to the non-branching nature of Micron.
4. Chicken Scheme can compile to C, which means that complicated applications can run on small boards like the Pi Zero. 

However, I realize that most people are probably not familiar with Scheme's syntax. If your goal is just to get a simple site up and running, I recommend just following a few Scheme tutorials to learn the absolute basics of the Syntax, and then copying one of the examples and modifying it to your liking. 

If you would like to implement new or complex functionality. I would recommend the following book on Scheme: [The Schematics of Computation by Manis and Little](https://www.math.purdue.edu/~lucier/schematics-front.pdf). I found it to be much more useful than SICP or "The Little Schemer", which are the books that people usually recommend. The Racket book [How to Design Programs](https://htdp.org/) is also a really good (and free) introduction to Scheme/Lisp languages.

## Developer Experience

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
