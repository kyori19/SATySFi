
open MyUtil

type build_state = {
  input_file             : abs_path;
  output_file            : abs_path option;
  type_check_only        : bool;
}

type test_state = {
  input_file_to_test  : abs_path;
}

type command_state =
  | BuildState of build_state
  | TestState  of test_state
  | SolveState

type state = {
  command_state      : command_state;
  extra_config_paths : (string list) option;
  show_full_path     : bool;
  no_default_config  : bool;
}

val set : state -> unit

val get : unit -> state

val does_show_full_path         : unit -> bool

val job_directory : unit -> string
