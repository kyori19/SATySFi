
open ConfigError
open MyUtil
open Types


val reset_pdf : unit -> unit


val evaluate_once :
  (unit -> unit) ->
  is_bytecomp_mode:bool ->
  environment ->
  abstract_tree ->
  (syntactic_value, config_error) result


val main :
  output_mode ->
  HandlePdf.config ->
  page_number_limit:int ->
  is_bytecomp_mode:bool ->
  environment ->
  abstract_tree ->
  abs_path ->
  abs_path option ->
  (unit, config_error) result
