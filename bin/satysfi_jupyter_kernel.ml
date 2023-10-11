
open Main__Debug
open Main__Types
open Main__Repl

external start_kernel : string -> unit = "start_kernel"

let execute ctx code =
  let result_to_string r =
    match r with
    | Ok(v) -> Printf.sprintf "[o] %s" (value_to_string v)
    | Error(EmptyInput) -> ""
    | Error(ParseError) -> "[x] Parse Error"
    | Error(TypecheckError _) -> "[x] Type Error"
    | _ -> "<not implemented>"
  in

  let result = feed ctx (Lexing.from_string code) in
  let output = result_to_string result in
  output

let () =
  let ctx = initialize () in
  Callback.register "satysfi_jupyter_kernel_execute" (execute ctx);
  let file_name = try Sys.argv.(1) with _ -> "connection.json" in
  start_kernel file_name
