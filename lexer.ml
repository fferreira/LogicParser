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

(* Managing source code positions *)

let initial_pos file_name = { Lexing.pos_fname = file_name ; 
			      Lexing.pos_lnum = 1 ; 
			      Lexing.pos_bol = 0 ; 
			      Lexing.pos_cnum = 0 }
let add_line pos = { pos with 
		     Lexing.pos_lnum = pos.Lexing.pos_lnum +1 ; 
		     Lexing.pos_bol = pos.Lexing.pos_bol + pos.Lexing.pos_cnum ; 
		     Lexing.pos_cnum = 0} 
let add_word pos length = { pos with Lexing.pos_cnum = pos.Lexing.pos_cnum + length }

(* The lexer *)

let rec main_scanner pos = lexer
| wsp | tab			-> main_scanner (add_word pos 1) lexbuf (* ignore whitespace *)
| nl				-> main_scanner (add_line pos) lexbuf   (* ignores new lines *)

| prop			        -> add_word pos (Ulexing.lexeme_length lexbuf), PROP (Ulexing.utf8_lexeme lexbuf)
| var			        -> add_word pos (Ulexing.lexeme_length lexbuf), VAR (Ulexing.utf8_lexeme lexbuf)
| 0x2227 (* ∧ *)		-> add_word pos (Ulexing.lexeme_length lexbuf), AND
| 0x2228 (* ∨ *)		-> add_word pos (Ulexing.lexeme_length lexbuf), OR
| 0x2200 (* ∀ *)		-> add_word pos (Ulexing.lexeme_length lexbuf), FORALL
| 0x2203 (* ∃ *)		-> add_word pos (Ulexing.lexeme_length lexbuf), EXISTS
| 0x2283 (* ⊃ *)                -> add_word pos (Ulexing.lexeme_length lexbuf), IMP 
| 0x00AC (* ¬ *)                -> add_word pos (Ulexing.lexeme_length lexbuf), NOT
| '.'				-> add_word pos (Ulexing.lexeme_length lexbuf), DOT
| '('				-> add_word pos (Ulexing.lexeme_length lexbuf), LPAREN
| ')'				-> add_word pos (Ulexing.lexeme_length lexbuf), RPAREN
| ';'				-> add_word pos (Ulexing.lexeme_length lexbuf), SEMICOLON
| eof				-> add_word pos (Ulexing.lexeme_length lexbuf), EOF
| _                             -> add_word pos (Ulexing.lexeme_length lexbuf), WHAT (Ulexing.utf8_lexeme lexbuf)
