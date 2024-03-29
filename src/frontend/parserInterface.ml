
module I = Parser.MenhirInterpreter

open MyUtil
open Types


let k_fail chkpt =
  match chkpt with
  | I.HandlingError(penv) ->
      let (lposS, lposE) = I.positions penv in
      let cnumS = lposS.Lexing.pos_cnum - lposS.Lexing.pos_bol in
      let cnumE = lposE.Lexing.pos_cnum - lposE.Lexing.pos_bol in
      let rng = Range.make lposS.Lexing.pos_fname lposS.Lexing.pos_lnum cnumS cnumE in
      raise (ParseError(CannotProgressParsing(rng)))

  | _ ->
      assert false


let lexbuf_from_file (abspath : abs_path) =
  let chan = open_in_abs abspath in
  let lexbuf = Lexing.from_channel chan in
  Lexing.set_filename lexbuf (get_abs_path_string abspath);
  lexbuf


let lexbuf_from_string (name : string) (s : string) =
  let lexbuf = Lexing.from_string s in
  Lexing.set_filename lexbuf name;
  lexbuf


let parse parser_start stack lexbuf =
  let open ResultMonad in
  let supplier = I.lexer_lexbuf_to_supplier (Lexer.cut_token stack) lexbuf in
  try
    return @@ I.loop_handle Fun.id k_fail supplier (parser_start lexbuf.Lexing.lex_curr_p)
  with
  | ParseError(e) ->
      err e


let parse_main =
  let stack = Lexer.reset_to_program () in
  parse Parser.Incremental.main stack


let parse_cell_main code =
  let stack = Lexer.initialize Lexer.CellState in
  let lexbuf = lexbuf_from_string "<input>" code in
  parse Parser.Incremental.cell_main stack lexbuf


let process_file (abspath : abs_path) =
  let lexbuf = lexbuf_from_file abspath in
  parse_main lexbuf


let process_text (abspath : abs_path) (s : string) =
  let lexbuf = lexbuf_from_string (get_abs_path_string abspath) s in
  parse_main lexbuf
