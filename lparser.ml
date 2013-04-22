exception Scanning_error of Lexing.position * string
exception Syntax_error of Lexing.position

let file_name = "pepe.pipi"

let parse menhir_parser lexbuf =
  (* TODO: columns are not being updated, Lexing module should not be used? *)
  let position = ref
      Lexing.({ pos_fname = file_name; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }) in
  let lexer () =
    let ante_position = !position in
    let nlines, token = Lexer.main_scanner 0 lexbuf in
    let () = position := Lexing.({!position with pos_lnum = !position.pos_lnum + nlines;}) in
    let post_position = !position
    in (token, ante_position, post_position) in
  let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser
  in try
       revised_parser lexer
    with
      | Lexer.Error x -> raise (Scanning_error (!position, x))
      | Parser.Error  -> raise (Syntax_error !position)

let file = ref ""
let args = []
let usage = "Usage: ./zurdo <options> [file] (stdin by default)"

let () =
  try
    Arg.parse args (fun s -> file := s) usage;
    let ch = if !file = "" then stdin else open_in !file in
    let lexbuf = Ulexing.from_utf8_channel ch in
    let c = parse Parser.props lexbuf in
    print_string (List.fold_right (fun x -> fun r -> (Syntax.to_string x) ^ "\n" ^ r) c "done\n")
  with
  | Syntax_error pos -> Printf.printf "Syntax error in line %d\n" pos.Lexing.pos_lnum

