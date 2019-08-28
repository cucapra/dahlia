if exists("b:current_syntax")
  finish
endif

" Numbers
syn match fuseConstant "\v<(\-|\+)?[0-9]+(\.[0-9]+)?(e[0-9]+)?>"
" Booleans
syn keyword fuseConstant true false
" Types
syn keyword fuseType ubit bit float bool bank double

" Control structures
syn keyword fuseKeyword if for while import
" Binding variables
syn keyword fuseKeyword let def decl extern as decor
syn keyword fuseKeyword unroll record combine
syn keyword fuseKeyword view split

syntax match fuseComment "\v//.*$"

hi link fuseKeyword  Keyword
hi link fuseConstant  Constant
hi link fuseComment  Comment
hi link fuseType Type

let b:current_syntax = "fuse"
