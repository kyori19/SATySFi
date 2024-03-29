
open MyUtil
open Types

val parse_main : Lexing.lexbuf -> (untyped_source_file, parse_error) result

val parse_cell_main : string -> (untyped_cell, parse_error) result

val process_file : abs_path -> (untyped_source_file, parse_error) result

val process_text : abs_path -> string -> (untyped_source_file, parse_error) result
