# Installation

To get Angstrom running on your system:

## Clone the repository

```
git clone https://github.com/pickles976/Macro.git
cd Macro
```
## Install Chicken Scheme and required packages  

```  

sudo apt install libssl-dev
sudo chicken-install sql-de-lite srfi-1 srfi-13 srfi-19 fmt http-client openssl  

```

## Build and install Angstrom modules  

```  

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

## Deploy to Nomadnet  

```  

# Copy pages to your Nomadnet storage directory
cp -r pages/* ~/.nomadnetwork/storage/pages/

# Make the main page executable
chmod +x ~/.nomadnetwork/storage/pages/index.mu  

```

## Update Paths  

You will need to update the paths of the app. If you are using relative imports, make sure that you are running `nomadnet` from the proper directory.
I personally prefer to use absolute paths.

## Generate the database tables  

```  

cd ~/.nomadnetwork/storage/pages
csi -s framework/manage.scm --generate  

```

Your Angstrom site is now live on your Nomadnet node! Access it through the Nomadnet interface.
