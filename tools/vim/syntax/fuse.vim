if exists("b:current_syntax")
  finish
endif

syntax keyword fuseKeyword if for while
syntax keyword fuseKeyword let def decl
syntax keyword fuseKeyword true false
syntax keyword fuseKeyword bank unroll record

syntax match fuseComment "\v//.*$"

highlight link fuseKeyword  Keyword
highlight link fuseComment  Comment
highlight link fuseString   String
highlight link fuseFuncDef  Function

let b:current_syntax = "fuse"
