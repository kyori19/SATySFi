
open Types
open StaticEnv
open TypeError

val typecheck_signature : Typeenv.t -> untyped_signature -> (signature abstracted, type_error) result

val typecheck_binding_list : Typeenv.t -> untyped_binding list -> (Typeenv.t * StructSig.t abstracted * binding list, type_error) result

val main : Typeenv.t -> (signature abstracted) option -> untyped_binding list -> (StructSig.t abstracted * binding list, type_error) result
