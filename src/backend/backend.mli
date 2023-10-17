
open MyUtil
open Types

type 'a t =
  (syntactic_value -> 'a) * (abs_path -> 'a -> unit)

val text : string t

val pdf : HandlePdf.t t
