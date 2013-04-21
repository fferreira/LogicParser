%{

open Syntax

%}

%token AND
%token OR
%token IMP
%token NOT
%token <string> PROP
%token <string> VAR
%token FORALL
%token EXISTS
%token DOT
%token LPAREN
%token RPAREN

%token SEMICOLON
%token EOF

%token <string> WHAT

%start <Syntax.prop list> props

%%

prop :
| p = PROP { Prop p }
| n = VAR { Var n }
| NOT p = prop { Not p }
| p1 = prop IMP p2 = prop { Imp (p1, p2) } 
| p1 = prop AND p2 = prop { And (p1, p2) } 
| p1 = prop OR  p2 = prop { Or  (p1, p2) } 
| FORALL x = VAR DOT p = prop { Forall (x, p) }
| EXISTS x = VAR DOT p = prop { Exists (x, p) }
| LPAREN p = prop RPAREN { p }
| c = WHAT { What c }

props : 
| EOF { [] }
| p = prop SEMICOLON ps=props { p :: ps }
