### Installation

To get Angstrom running on your system:

**Clone the repository**  

   ```bash
   git clone https://github.com/pickles976/Macro.git
   cd Macro
   ```

**Install Chicken Scheme**  

   - On Debian/Ubuntu: `sudo apt-get install chicken-bin`
   - On Arch: `sudo pacman -S chicken`
   - On macOS: `brew install chicken`

**Install required Chicken Scheme packages**  

   ```bash
   sudo chicken-install sql-de-lite srfi-1 srfi-13 srfi-19
   ```

**Build and install Angstrom modules**  

   ```bash
   cd pages/framework

   # Build the modules
   csc -s micron.scm
   csc -s markdown.scm
   csc -s orm-lib.scm

   # Install system-wide (optional)
   sudo chicken-install -s micron.egg
   sudo chicken-install -s markdown.egg
   sudo chicken-install -s orm.egg

   cd ../..
   ```

   After installation:
   ```scheme
   (import micron)
   (import markdown)
   (import orm)
   ```

**Deploy to Nomadnet**  

   ```bash
   # Copy pages to your Nomadnet storage directory
   cp -r pages/* ~/.nomadnetwork/storage/pages/

   # Make the main page executable
   chmod +x ~/.nomadnetwork/storage/pages/index.mu
   ```

**Generate the database tables**  

   ```bash
   cd ~/.nomadnetwork/storage/pages
   csi -s framework/manage.scm --generate
   ```

Your Angstrom site is now live on your Nomadnet node! Access it through the Nomadnet interface.

---

If this section of the page looks weird, that's because it was translated from markdown into micron when the page loaded! Load markdown directly into your nomadnet page!

---

