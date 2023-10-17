
open MyUtil
open LengthInterface
open HorzBox

type t

val create_empty_pdf : unit -> t

type page

val write_page : page -> page_parts_scheme_func -> t -> t

val write_to_file : abs_path -> t -> unit

val make_empty_page : paper_size:(length * length) -> page_break_info -> page_content_scheme -> page

val add_column_to_page : page -> length -> evaled_vert_box list -> evaled_vert_box list -> page
