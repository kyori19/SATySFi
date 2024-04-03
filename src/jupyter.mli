
open StaticEnv
open Types


type eval_config = {
  display_config : Logging.config;
  output_mode : output_mode;
  typecheck_config : typecheck_config;
  pdf_config : HandlePdf.config;
  page_number_limit : int;
  is_bytecomp_mode : bool;
  fnast : abstract_tree;
}


val main : eval_config -> type_environment -> environment -> string -> unit
