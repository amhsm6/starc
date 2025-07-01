" Vim syntax file
" Language: Starlang
" Maintainer: Gemini
" Last Change: 2025-07-01

if exists("b:current_syntax")
  finish
endif

syntax case match

" Keywords
syntax keyword starKeywords fn return if else

" Types
syntax keyword starTypes int8 int16 int32 int64 bool

" Built-in functions
syntax keyword starBuiltins print_int8 print_int16 print_int32 print_int64 print_bool

" Booleans
syntax keyword starBoolean true false

" Operators
syntax match starOperator "\(&&\|||\)"
syntax match starOperator "!"
syntax match starOperator ":="
syntax match starOperator "=="
syntax match starOperator "!="
syntax match starOperator ">="
syntax match starOperator "<="
syntax match starOperator ">"
syntax match starOperator "<"
syntax match starOperator "="
syntax match starOperator "\*"
syntax match starOperator "/"
syntax match starOperator "+"
syntax match starOperator "-"
syntax match starOperator "&"

syntax match starFunction "\w\+(\@="

" Numbers
syntax match starNumber "\<\d\+\>"

" Comments
syntax match starComment "//.*"

" Strings (assuming double quotes)
syntax region starString start=/"/ end=/"/

" Highlighting links
highlight link starKeywords Keyword
highlight link starTypes Type
highlight link starBuiltins Function
highlight link starFunction Function
highlight link starBoolean Boolean
highlight link starOperator Operator
highlight link starNumber Number
highlight link starComment Comment
highlight link starString String

let b:current_syntax = "starlang"
