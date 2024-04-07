type t = string

(*
  <https://erratique.ch/software/uunf/doc/Uunf/index.html#type-form>

*)
let of_utf8_nfd str = Uunf_string.normalize_utf_8 `NFD str
let of_utf8_nfc str = Uunf_string.normalize_utf_8 `NFC str

let of_utf16be_nfd str =
  let str_utf8 = str |> InternalText.of_utf16be |> InternalText.to_utf8 in
  Uunf_string.normalize_utf_8 `NFD str_utf8

let of_utf16be_nfc str =
  let str_utf8 = str |> InternalText.of_utf16be |> InternalText.to_utf8 in
  Uunf_string.normalize_utf_8 `NFC str_utf8

let to_utf8 t = t

let to_utf16be t =
  t |> InternalText.of_utf8 |> InternalText.to_utf16be

let to_utf16be_hex t =
  t |> InternalText.of_utf8 |> InternalText.to_utf16be_hex
