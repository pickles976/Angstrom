# README

### TODO
- [x] get a basic page with comments working
- [ ] recreate it in chicken scheme
    - [ ] simple DSL
        - [x] links
        - [x] fields
        - [x] submit buttons
        - [x] improved text styling (check deepseek's suggestions)
        - [x] style should not take any arguments
        - [x] consolidate my DSL implementation with the one Deepseek provided
    - [ ] user interaction
        - [ ] load comments from file
        - [ ] write comments to file
        - [ ] hooray! feature-parity achieved!

- [ ] create an "actions" file which can do multiple things
- [ ] switch from file-based comments, to sqlite-based comment system
- [ ] update my personal node with this

- [ ] integrate recipe search
    - [ ] run hari.recipes locally
    - [ ] query the API and html
    - [ ] parse and list
    - [ ] click on list entry and load HTML
    - [ ] convert HTML to micron
- [ ] create epub search

https://wiki.call-cc.org/chicken-for-python-programmers

`sudo apt-get install chicken-bin`

```
csi -s ./framework/pages/index.mu 
```

`sudo chicken-install srfi-1`

# Compile the module
`csc -s -J micron-dsl.scm`

# Install it
chicken-install micron-dsl