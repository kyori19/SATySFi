
open Length
open Types

let value_to_string v =
  match v with
  | BaseConstant(BCUnit) -> "()"
  | BaseConstant(BCBool b) -> string_of_bool b
  | BaseConstant(BCInt i) -> string_of_int i
  | BaseConstant(BCFloat f) -> string_of_float f
  | BaseConstant(BCString s) -> "`" ^ s ^ "`"
  | BaseConstant(BCLength l) -> string_of_float (to_pdf_point l) ^ "pt"
  | _ -> "<not implemented>"
