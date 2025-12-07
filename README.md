# Angstrom

Tools for building Nomadnet apps in Chicken Scheme. Includes an ORM, a micron DSL, and a markdown converter.

## Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/pickles976/Angstrom.git
   cd Angstrom
   ```

2. **Install Chicken Scheme and dependencies**
   ```bash
   sudo apt-get install chicken-bin
   sudo chicken-install sql-de-lite srfi-1 srfi-13 srfi-19 fmt
   ```

3. **Build and install Angstrom modules**
   ```bash
   cd framework

   # Remove old versions (if updating)
   sudo chicken-uninstall micron
   sudo chicken-uninstall markdown
   sudo chicken-uninstall orm

   # Build and install all modules
   sudo csc -s micron.scm -J && sudo chicken-install
   sudo csc -s markdown.scm -J && sudo chicken-install
   sudo csc -s orm-lib.scm -J && sudo chicken-install
   ```

4. **Update Paths**

You will need to update the paths of the app. If you are using relative imports, make sure that you are running `nomadnet` from the proper directory. 
I personally prefer to use absolute paths.

## Building a Project

Copy the contents of `pages` to `~/.nomadnetwork/storage`
Run `chmod +x` on any pages or sub-pages that will be run as scripts.
Or just do:
`chmod -R 755 ~/.nomadnetwork/storage/pages`

### If Using the ORM

Generate the sqlite file from your `models.scm` file:
```bash
csi -s framework/manage.scm --generate \
    --db-path /absolute/path/to/app.db \
    --models-path /absolute/path/to/models.scm
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

### VS Code Setup

1. **Install the Scheme LSP Extension**
   ```bash
   code --install-extension rgherdt.scheme-lsp
   ```

2. **Install the Chicken Scheme LSP Server**
   ```bash
   sudo chicken-install lsp-server
   ```

   This will install the `chicken-lsp-server` binary that provides:
   - Auto-completion
   - Go to definition (F12)
   - Hover documentation
   - Signature help

3. **Configure Your Workspace**

   Create `.vscode/settings.json` in your project root:
   ```json
   {
     "scheme-lsp.schemeLspImplementation": "chicken",
     "files.associations": {
       "*.scm": "scheme",
       "*.sld": "scheme",
       "*.mu": "scheme"
     },
     "editor.formatOnSave": false,
     "editor.quickSuggestions": {
       "other": true,
       "comments": false,
       "strings": false
     }
   }
   ```

4. **Optional: Install chicken-doc for Better Completions**
   ```bash
   sudo chicken-install -s apropos chicken-doc srfi-18
   cd `csi -R chicken.platform -p '(chicken-home)'`
   curl http://3e8.org/pub/chicken-doc/chicken-doc-repo.tgz | sudo tar zx
   ```

5. **Reload VS Code**
   - Press `Ctrl+Shift+P` and type "Reload Window"
   - Or restart VS Code

The LSP should now activate when you open any `.scm`, `.sld`, or `.mu` file.

### Helix Setup

1. **Install the Chicken Scheme LSP Server**
   ```bash
   sudo chicken-install lsp-server
   ```

   This installs the `chicken-lsp-server` binary system-wide.

2. **Configure Helix**

   Create or edit `~/.config/helix/languages.toml`:
   ```toml
   [[language]]
   name = "scheme"
   language-servers = ["chicken-lsp"]
   file-types = ["scm", "sld", "mu"]

   [language-server.chicken-lsp]
   command = "chicken-lsp-server"
   args = ["--stdio"]
   ```

3. **Optional: Install chicken-doc for Better Completions**
   ```bash
   sudo chicken-install -s apropos chicken-doc srfi-18
   cd `csi -R chicken.platform -p '(chicken-home)'`
   curl http://3e8.org/pub/chicken-doc/chicken-doc-repo.tgz | sudo tar zx
   ```

   This provides documentation for Chicken Scheme's built-in functions.

4. **Verify Configuration**
   ```bash
   hx --health scheme
   ```

   You should see a green checkmark next to "chicken-lsp" in the configured language servers.

5. **LSP Features in Helix**
   - **Auto-completion**: Press `Ctrl+x` (or it appears automatically)
   - **Go to definition**: Press `gd`
   - **Hover documentation**: Press `K` (shift+k)
   - **Rename symbol**: Press `Space+r`
   - **Show references**: Press `Space+a`

The LSP will activate automatically when you open any `.scm`, `.sld`, or `.mu` file in Helix.

#### Zip Command

`tar --exclude=./NomadnetTemplates/.git -czvf Angstrom.tar.gz ./NomadnetTemplates`