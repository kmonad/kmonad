# Module structure

#### Description

In `kmonad` I am trying to think in these terms:
- A module is a logical chunk of code.
- A module may exist as a single file, or as a collection of files.
- The collection of files should follow the following rules:
  - There is always an initial `./Module/Initial.hs`
    - This file may not import anything internal to the module
    - Therefore it is safe to import into any module-file
    - This file may import anything trunkwards in the module tree
    - It is responsible for setting up the 'prelude' to a module 
  - Likewise there is always a final `./Module.hs` file
    - This file may not be imported internally
    - It is what you import when you want to access the module
    - It may import anything leafwards in the module tree
  - Imports between files in the submodule are up to programmer discretion.

#### Example
Consider the following:
```
.
├── Bar.hs
├── Foo
│   ├── Initial.hs
│   ├── IO.hs
│   ├── Util
│   │   ├── Extras.hs
│   │   └── Initial.hs
│   └── Util.hs
└── Foo.hs
```

Here:
- `Bar.hs` is a top-level, 1-file module
- `Foo` is a top-level, multifile module with nested submodules
- `Util` is a submodule of `Foo`

CONTINUE HERE
