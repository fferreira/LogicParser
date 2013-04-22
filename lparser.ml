exception Scanning_error of Lexing.position * string
exception Syntax_error of Lexing.position

let file_name = "pepe.pipi"

let parse menhir_parser lexbuf =
  let position = ref (Lexer.initial_pos file_name) in
  let lexer () =
    let ante_position = !position in
    let post_position, token = Lexer.main_scanner !position lexbuf in
    let () = position := post_position in (* Lexing.({!position with pos_lnum = !position.pos_lnum + nlines;}) in *)
    (token, ante_position, post_position) in
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
  | Syntax_error pos -> Printf.printf "Syntax error in line %d, col %d\n" pos.Lexing.pos_lnum pos.Lexing.pos_cnum

