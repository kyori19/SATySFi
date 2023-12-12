
open MyUtil
open Types
open StaticEnv
open ConfigError

val typecheck_document_file : bool -> type_environment -> abs_path -> untyped_abstract_tree -> (abstract_tree, config_error) result

val main : type_environment -> global_type_environment -> untyped_package -> (struct_signature * (abs_path * binding list) list, config_error) result

val main_document : type_environment -> global_type_environment -> (abs_path * untyped_library_file) list -> header_element list -> ((abs_path * binding list) list * type_environment, config_error) result
