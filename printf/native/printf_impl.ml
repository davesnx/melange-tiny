external format_float : string -> float -> string = "caml_format_float"

let float_g p f = format_float ("%." ^ string_of_int p ^ "g") f
