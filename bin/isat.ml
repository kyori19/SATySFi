
open Main__Debug
open Main__Repl

let () =
  let ctx = initialize () in

  let prompt s =
    Printf.printf "%s> %!" s;
    try read_line () with
    | End_of_file ->
      Printf.printf "\n";
      exit 0
  in

  let result_to_string r =
    match r with
    | Ok(v) -> Printf.sprintf "[o] %s" (value_to_string v)
    | Error(EmptyInput) -> ""
    | Error(ParseError) -> "[x] Parse Error"
    | Error(TypecheckError _) -> "[x] Type Error"
    | _ -> "<not implemented>"
  in

  let rec loop () =
    let input = prompt "isat" in
    let result = feed ctx (Lexing.from_string input) in
    let output = result_to_string result in
    if String.length output > 0 then
      Printf.printf "%s\n%!" output;
    loop ()
  in

  loop ()
