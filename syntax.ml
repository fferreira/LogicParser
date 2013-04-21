
type prop = Prop of string
  | Var of string
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Imp of prop * prop
  | Forall of string * prop
  | Exists of string * prop
  | Top
  | Bot
  | What of string


let rec to_string e = match e with
| Prop p -> p
| Var v -> v
| Top -> "⊤"
| Bot -> "⊥"
| Not p -> "¬" ^ to_string p
| And (p1, p2) -> "(" ^ to_string p1 ^ " ∧ " ^ to_string p2 ^ ")"
| Or (p1, p2) -> "(" ^ to_string p1 ^ " ∨ " ^ to_string p2 ^ ")"
| Imp (p1, p2) -> "(" ^ to_string p1 ^ " ⊃ " ^ to_string p2 ^ ")"
| Forall (v, p) -> "∀" ^ v ^ "." ^ to_string p
| Exists (v, p) -> "∃" ^ v ^ "." ^ to_string p
| What s -> "Unrecognized: " ^ s

