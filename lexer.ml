open Parser

exception Error of string

let regexp nl  = ('\n' | '\r' | "\r\n" | "\n\r")
let regexp tab = ['\t''\x0b']
let regexp wsp = [' ''\t']

let regexp numeral = ['0' - '9']+

let regexp lower = ['a'-'z']
let regexp upper = ['A'-'Z']

let regexp var = lower (lower | upper)*
let regexp prop = upper (lower | upper)*

let rec main_scanner nlines = lexer
| wsp | tab			-> main_scanner nlines lexbuf         (* ignore whitespace *)
| nl				-> main_scanner (succ nlines) lexbuf   (* ignores new lines *)

| prop			        -> nlines, PROP (Ulexing.utf8_lexeme lexbuf)
| var			        -> nlines, VAR (Ulexing.utf8_lexeme lexbuf)
| 0x2227 (* ∧ *)		-> nlines, AND
| 0x2228 (* ∨ *)		-> nlines, OR
| 0x2200 (* ∀ *)		-> nlines, FORALL
| 0x2203 (* ∃ *)		-> nlines, EXISTS
| 0x2283 (* ⊃ *)                -> nlines, IMP 
| 0x00AC (* ¬ *)                -> nlines, NOT
| '.'				-> nlines, DOT
| '('				-> nlines, LPAREN
| ')'				-> nlines, RPAREN
| ';'				-> nlines, SEMICOLON
| eof				-> nlines, EOF
| _                             -> nlines, WHAT (Ulexing.utf8_lexeme lexbuf)
