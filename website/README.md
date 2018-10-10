Seashell Demo
-------------

This folder contains the code to generate the client-side demo for Seashell.

To build the demo, go to the top of the repository and run `make demo`.
This will generate a JS binary generated using js_of_ocaml. The generated
is copied over to this directory and used for the client side compiler.

### Requirements

1. Install `yarn` for JS package management.
2. Run `yarn install` to install deps.
3. Run `yarn build` if making local changes. Run `make demo` from repo top if
   changing ocaml.
