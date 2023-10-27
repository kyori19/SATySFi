
external start_kernel : string -> unit = "start_kernel"

let execute _ =
  "<not implemented>"

let main connection_file =
  Callback.register "satysfi_jupyter_kernel_execute" execute;
  let file_name =
    match connection_file with
    | None -> "connection.json"
    | Some file_name -> file_name
  in
  start_kernel file_name
