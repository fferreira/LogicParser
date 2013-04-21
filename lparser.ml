exception Scanning_error of Lexing.position * string
exception Syntax_error of Lexing.position

let file_name = "pepe.pipi"

let parse menhir_parser lexbuf =
  let position = ref
      Lexing.({ pos_fname = file_name; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }) in
  let lexer () =
    let ante_position = !position in
    let nlines, token = Lexer.main_scanner 1 lexbuf in
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
  Arg.parse args (fun s -> file := s) usage;
  let ch = if !file = "" then stdin else open_in !file in
  let lexbuf = Ulexing.from_utf8_channel ch in
  let c = parse Parser.props lexbuf in
  print_string (List.fold_right (fun x -> fun r -> (Syntax.to_string x) ^ "\n" ^ r) c "done\n")























(* exception Caramba of string *)





(* (\** Parser wrapper *\) *)
(* let parse parser lex = *)
(*   try *)
(*     parser Lexer.token lex *)
(*   with *)
(*   | Parser.Error -> *)
(*       (\* Error.syntax ~loc:(Lexer.position_of_lex lex) "" *\) *)
(*       raise (Caramba "parse error") *)
(*   | Failure "lexing: empty token" -> *)
(*       (\* Error.syntax ~loc:(Lexer.position_of_lex lex) "unrecognised symbol." *\) *)
(*       raise (Caramba "lexer error") *)

(* (\** Interactive toplevel *\) *)
(* let toplevel () = *)
(*   let eof = match Sys.os_type with *)
(*     | "Unix" | "Cygwin" -> "Ctrl-D" *)
(*     | "Win32" -> "Ctrl-Z" *)
(*     | _ -> "EOF" *)
(*   in *)
(*   print_endline ("zurdo " ^ "0.00"); *)
(*   print_endline ("[Type " ^ eof ^ " to exit or \"Help.\" for help.]"); *)
(*   try *)
(*     while true do *)
(*       try *)
(* 	let pp = parse Parser.expressions in *)
(*         let _cmds = Lexer.read_toplevel pp () in *)
(* 	() *)
(*       with *)
(*         (\* | Error.Error err -> Print.error err *\) *)
(*         | Sys.Break -> prerr_endline "Interrupted." *)
(* 	| _ -> prerr_endline "oops"  *)
(*     done *)
(*   with End_of_file -> () *)

(* let main = Sys.catch_break true ; toplevel () *)
