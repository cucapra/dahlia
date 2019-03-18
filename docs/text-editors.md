---
id: text-editors
title: Text Editors
---

This section details the currently existing text editor support for Fuse.

## Vim

We have a syntax highlighting plugin called `vim-fuse`.

- If you're using [Pathogen](https://github.com/tpope/vim-pathogen), install
  `vim-fuse` by running this in the `seashell/` directory:
    - `cp -r tools/vim ~/.vim/bundle/vim-fuse`
    
## Emacs

We have a mode that provides syntax highlighting and indentation.

Add `tools/fuse-mode` to your emacs load-path and then require `fuse-mode`.
For example, if the `Seashell` repo is located at `~/seashell`. Then you would
add the following lines to your emacs init file:
```lisp
(push "~/seashell/tools/emacs/fuse-mode")
(require 'fuse-mode)
```
