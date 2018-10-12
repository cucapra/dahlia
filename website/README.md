Seashell Demo
-------------

This folder contains the code to generate the UI for the  client-side demo for
Seashell.

### Build Instructions

The following commands only rebuild the UI code. If the ocaml code changes,
navigate to the repository's top and run `make local-website` and `firefox
website/dist/index.html` to view the updated demo page.

1. Install `yarn` for JS package management.
2. Run `yarn install` to install deps.
3. Run `yarn build` if making local changes. Run `make demo` from repo top if
   changing ocaml.
