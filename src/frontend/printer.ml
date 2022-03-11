let print_value x =
  let rec print = function
    | Types.Nil -> "[]"
    | BaseConstant c -> print_constant c
    | Constructor (name, value) ->
      Printf.sprintf "%s %s" name (print value)
    | List vs ->
      vs |> List.map print |> String.concat ", " |> Printf.sprintf "[%s]"
    | Tuple vs ->
      vs |> List.map print |> String.concat ", " |> Printf.sprintf "(%s)"
    | RecordValue vs ->
      vs |> SyntaxBase.LabelMap.bindings |> List.map (fun (k, v) -> k ^ " = " ^ print v) |> String.concat ", " |> Printf.sprintf "(| %s |)"
    | Location _ ->
      Printf.sprintf "<location>"
    | Context _ ->
      "<ctx>"
    | CodeSymbol _ ->
      "<code symbol>"
    | _ ->
      "<unknown>"
  and print_constant = function
    | BCUnit -> "()"
    | BCBool b -> Bool.to_string b
    | BCInt i -> Int.to_string i
    | BCFloat f -> Float.to_string f
    | x -> Types.show_base_constant x
  in
  print x

let print_mono_type _ = "(* TODO *)"
