open CamlinternalFormatBasics

type ('b, 'c) acc =
  | End
  | Str of ('b, 'c) acc * string
  | Delay of ('b, 'c) acc * ('b -> 'c)
  | Flush of ('b, 'c) acc
  | Invalid of ('b, 'c) acc * string

external chr : char -> string = "fromCharCode" [@@mel.scope "String"]
external repeat : string -> int -> string = "repeat" [@@mel.send]
external slice : string -> int -> string = "slice" [@@mel.send]
external sub : string -> int -> int -> string = "slice" [@@mel.send]
external upper : string -> string = "toUpperCase" [@@mel.send]
external index_of : string -> string -> int = "indexOf" [@@mel.send]
external parse_int : string -> int = "parseInt"
external is_finite : float -> bool = "isFinite"
external to_fixed : float -> int -> string = "toFixed" [@@mel.send]
external to_fixed0 : float -> string = "toFixed" [@@mel.send]
external to_exponential : float -> int -> string = "toExponential" [@@mel.send]

external hexstring_of_float : float -> int -> char -> string
  = "caml_hexstring_of_float"

(* Melange represents int32 and nativeint as 32-bit JS numbers, like int, and
   an int64 as the array [hi; lo] of two 32-bit words: see type [t] in
   melange/js/melange/caml_int64.ml. *)
type int64_words = { hi : int; [@mel.as "0"] lo : int [@mel.as "1"] }

external words : int64 -> int64_words = "%identity"
external int32_bits : int32 -> int = "%identity"
external nativeint_bits : nativeint -> int = "%identity"

let get = String.unsafe_get
let abs x = if x < 0 then -x else x
let impossible () = assert false

let escape quote s =
  let r = ref "" in
  for i = 0 to String.length s - 1 do
    let c = get s i in
    r :=
      !r
      ^
      match c with
      | '\\' -> "\\\\"
      | '\n' -> "\\n"
      | '\t' -> "\\t"
      | '\r' -> "\\r"
      | '\b' -> "\\b"
      | c when c = quote -> "\\" ^ chr c
      | ' ' .. '~' -> chr c
      | c ->
          let n = Char.code c in
          "\\"
          ^ chr (Char.unsafe_chr (48 + (n / 100)))
          ^ chr (Char.unsafe_chr (48 + (n / 10 mod 10)))
          ^ chr (Char.unsafe_chr (48 + (n mod 10)))
  done;
  !r

let fix_padding padty width str =
  let len = String.length str in
  let width, padty = (abs width, if width < 0 then Left else padty) in
  if width <= len then str
  else
    let fill c = repeat c (width - len) in
    match padty with
    | Left -> str ^ fill " "
    | Right -> fill " " ^ str
    | Zeros when len > 0 && (get str 0 = '+' || get str 0 = '-' || get str 0 = ' ')
      ->
        sub str 0 1 ^ fill "0" ^ slice str 1
    | Zeros
      when len > 1 && get str 0 = '0' && (get str 1 = 'x' || get str 1 = 'X') ->
        sub str 0 2 ^ fill "0" ^ slice str 2
    | Zeros -> fill "0" ^ str

let fix_int_precision prec str =
  let prec = abs prec in
  let len = String.length str in
  match get str 0 with
  | ('+' | '-' | ' ') when prec + 1 > len ->
      sub str 0 1 ^ repeat "0" (prec - len + 1) ^ slice str 1
  | '0' when prec + 2 > len && len > 1 && (get str 1 = 'x' || get str 1 = 'X') ->
      sub str 0 2 ^ repeat "0" (prec - len + 2) ^ slice str 2
  | c
    when prec > len
         && ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
    ->
      repeat "0" (prec - len) ^ str
  | _ -> str

let rec digits base a b c d s =
  let b = (mod_float a base *. 65536.) +. b in
  let c = (mod_float b base *. 65536.) +. c in
  let d = (mod_float c base *. 65536.) +. d in
  let r = int_of_float (mod_float d base) in
  let a = floor (a /. base)
  and b = floor (b /. base)
  and c = floor (c /. base)
  and d = floor (d /. base) in
  let s = sub "0123456789abcdef" r (r + 1) ^ s in
  if a +. b +. c +. d = 0. then s else digits base a b c d s

let signed = function
  | Int_u | Int_Cu | Int_x | Int_Cx | Int_X | Int_CX | Int_o | Int_Co -> false
  | Int_d | Int_pd | Int_sd | Int_i | Int_pi | Int_si | Int_Cd | Int_Ci -> true

let convert_bits iconv hi lo =
  let neg = signed iconv && hi < 0 in
  let lo' = if neg then -lo else lo in
  let hi = if neg then (hi lxor -1) + if lo' = 0 then 1 else 0 else hi in
  let base =
    match iconv with
    | Int_x | Int_Cx | Int_X | Int_CX -> 16.
    | Int_o | Int_Co -> 8.
    | _ -> 10.
  in
  let s =
    digits base
      (float (hi lsr 16))
      (float (hi land 0xffff))
      (float (lo' lsr 16))
      (float (lo' land 0xffff))
      ""
  in
  let s =
    (if neg then "-"
     else
       match iconv with
       | Int_pd | Int_pi -> "+"
       | Int_sd | Int_si -> " "
       | _ -> "")
    ^ (match iconv with Int_Cx | Int_CX -> "0x" | Int_Co -> "0" | _ -> "")
    ^ match iconv with Int_X | Int_CX -> upper s | _ -> s
  in
  match iconv with
  | Int_Cd | Int_Ci | Int_Cu ->
      let n = ref 0 in
      for i = 0 to String.length s - 1 do
        match get s i with '0' .. '9' -> incr n | _ -> ()
      done;
      let left = ref (((!n - 1) mod 3) + 1) and r = ref "" in
      for i = 0 to String.length s - 1 do
        match get s i with
        | '0' .. '9' as c ->
            if !left = 0 then (
              r := !r ^ "_";
              left := 3);
            decr left;
            r := !r ^ chr c
        | c -> r := !r ^ chr c
      done;
      !r
  | _ -> s

let convert_int iconv n =
  convert_bits iconv (if signed iconv then n asr 31 else 0) n

let convert_int32 iconv n = convert_int iconv (int32_bits n)
let convert_nativeint iconv n = convert_int iconv (nativeint_bits n)

let convert_int64 iconv n =
  let w = words n in
  convert_bits iconv w.hi w.lo

let pad_exp s =
  let i = String.length s in
  if get s (i - 3) = 'e' then sub s 0 (i - 1) ^ "0" ^ slice s (i - 1) else s

let format_float conv prec x =
  if x <> x then "nan"
  else if not (is_finite x) then "inf"
  else
    match conv with
    | 'e' -> pad_exp (to_exponential x prec)
    | 'f' -> to_fixed x prec
    | _ ->
        let prec = if prec <> 0 then prec else 1 in
        let s = to_exponential x (prec - 1) in
        let j = index_of s "e" in
        let exp = parse_int (slice s (j + 1)) in
        if exp < -4 || x >= 1e21 || String.length (to_fixed0 x) > prec then begin
          let i = ref (j - 1) in
          while get s !i = '0' do
            decr i
          done;
          if get s !i = '.' then decr i;
          pad_exp (sub s 0 (!i + 1) ^ slice s j)
        end
        else
          let rec fixed p =
            let s = to_fixed x p in
            if exp >= 0 && String.length s > prec + 1 then fixed (p - 1)
            else if p = 0 then s
            else
              let rec strip k =
                if get s k = '0' then strip (k - 1)
                else sub s 0 (if get s k = '.' then k else k + 1)
              in
              strip (String.length s - 1)
          in
          fixed (if exp < 0 then prec - (exp + 1) else prec)

let convert_float (flag, kind) prec x =
  let sign =
    match flag with Float_flag_p -> '+' | Float_flag_s -> ' ' | Float_flag_ -> '-'
  in
  let special str =
    if x <> x then "nan"
    else if is_finite x then str
    else if x < 0. then "neg_infinity"
    else "infinity"
  in
  let decimal conv up =
    let p = abs prec in
    let s = format_float conv (if p < 0 then 0 else p) (abs_float x) in
    (if x < 0. then "-" else if sign = '-' then "" else chr sign)
    ^ if up then upper s else s
  in
  match kind with
  | Float_h -> hexstring_of_float x prec sign
  | Float_H -> upper (hexstring_of_float x prec sign)
  | Float_CF -> special (hexstring_of_float x prec sign)
  | Float_F ->
      let s = decimal 'g' false in
      let rec has_dot i =
        i < String.length s
        && (match get s i with '.' | 'e' | 'E' -> true | _ -> has_dot (i + 1))
      in
      special (if has_dot 0 then s else s ^ ".")
  | Float_f -> decimal 'f' false
  | Float_e -> decimal 'e' false
  | Float_E -> decimal 'e' true
  | Float_g -> decimal 'g' false
  | Float_G -> decimal 'g' true

let string_of_formatting_lit = function
  | Close_box -> "@]"
  | Close_tag -> "@}"
  | Break (s, _, _) | Magic_size (s, _) -> s
  | FFlush -> "@?"
  | Force_newline -> "@\n"
  | Flush_newline -> "@."
  | Escaped_at -> "@@"
  | Escaped_percent -> "@%"
  | Scan_indic c -> "@" ^ chr c

let rec string_of_fmtty :
    type a b c d e f g h i j k l.
    (a, b, c, d, e, f, g, h, i, j, k, l) fmtty_rel -> string = function
  | Char_ty r -> "%c" ^ string_of_fmtty r
  | String_ty r -> "%s" ^ string_of_fmtty r
  | Int_ty r -> "%i" ^ string_of_fmtty r
  | Int32_ty r -> "%li" ^ string_of_fmtty r
  | Nativeint_ty r -> "%ni" ^ string_of_fmtty r
  | Int64_ty r -> "%Li" ^ string_of_fmtty r
  | Float_ty r -> "%f" ^ string_of_fmtty r
  | Bool_ty r -> "%B" ^ string_of_fmtty r
  | Alpha_ty r -> "%a" ^ string_of_fmtty r
  | Theta_ty r -> "%t" ^ string_of_fmtty r
  | Any_ty r -> "%?" ^ string_of_fmtty r
  | Reader_ty r -> "%r" ^ string_of_fmtty r
  | Ignored_reader_ty r -> "%_r" ^ string_of_fmtty r
  | Format_arg_ty (s, r) ->
      "%{" ^ string_of_fmtty s ^ "%}" ^ string_of_fmtty r
  | Format_subst_ty (s, _, r) ->
      "%(" ^ string_of_fmtty s ^ "%)" ^ string_of_fmtty r
  | End_of_fmtty -> ""

type ty = Ty : (_, _, _, _, _, _, _, _, _, _, _, _) fmtty_rel -> ty [@@unboxed]

(* The Melange runtime identifies an exception by its MEL_EXN_ID string, and
   camlinternalFormat.ml creates Type_mismatch once, with
   Caml_exceptions.create, which gives the id
   "CamlinternalFormat.Type_mismatch/1". *)
external melange_error : string -> exn = "MelangeError"
[@@mel.new] [@@mel.module "melange.js/caml_js_exceptions.js"]

let[@inline never] mismatch () = raise (melange_error "CamlinternalFormat.Type_mismatch/1")
let same a b = if string_of_fmtty a <> string_of_fmtty b then mismatch ()

let step :
    type a b c d e f g h i j k l.
    (a, b, c, d, e, f, g, h, i, j, k, l) fmtty_rel -> int * ty = function
  | Char_ty r -> (0, Ty r)
  | String_ty r -> (1, Ty r)
  | Int_ty r -> (2, Ty r)
  | Int32_ty r -> (3, Ty r)
  | Nativeint_ty r -> (4, Ty r)
  | Int64_ty r -> (5, Ty r)
  | Float_ty r -> (6, Ty r)
  | Bool_ty r -> (7, Ty r)
  | Alpha_ty r -> (8, Ty r)
  | Theta_ty r -> (9, Ty r)
  | Any_ty r -> (10, Ty r)
  | Reader_ty r -> (11, Ty r)
  | Ignored_reader_ty r -> (12, Ty r)
  | Format_arg_ty (_, r) -> (13, Ty r)
  | Format_subst_ty (_, _, r) -> (14, Ty r)
  | End_of_fmtty -> (15, Ty End_of_fmtty)

let expect k (Ty t) =
  let k', rest = step t in
  if k' <> k then mismatch ();
  rest

let arg_pad : type x y. (x, y) padding -> ty -> ty =
 fun p ty -> match p with Arg_padding _ -> expect 2 ty | _ -> ty

let arg_prec : type x y. (x, y) precision -> ty -> ty =
 fun q ty -> match q with Arg_precision -> expect 2 ty | _ -> ty

let rec type_fmt : type a b c d e f. (a, b, c, d, e, f) fmt -> ty -> ty =
 fun fmt ty ->
  match fmt with
  | Char r -> type_fmt r (expect 0 ty)
  | Caml_char r -> type_fmt r (expect 0 ty)
  | String (p, r) -> type_fmt r (expect 1 (arg_pad p ty))
  | Caml_string (p, r) -> type_fmt r (expect 1 (arg_pad p ty))
  | Int (_, p, q, r) -> type_fmt r (expect 2 (arg_prec q (arg_pad p ty)))
  | Int32 (_, p, q, r) -> type_fmt r (expect 3 (arg_prec q (arg_pad p ty)))
  | Nativeint (_, p, q, r) -> type_fmt r (expect 4 (arg_prec q (arg_pad p ty)))
  | Int64 (_, p, q, r) -> type_fmt r (expect 5 (arg_prec q (arg_pad p ty)))
  | Float (_, p, q, r) -> type_fmt r (expect 6 (arg_prec q (arg_pad p ty)))
  | Bool (p, r) -> type_fmt r (expect 7 (arg_pad p ty))
  | Flush r | String_literal (_, r) | Char_literal (_, r) | Formatting_lit (_, r)
    ->
      type_fmt r ty
  | Format_arg (_, s, r) -> (
      match ty with
      | Ty (Format_arg_ty (s', t)) ->
          same s s';
          type_fmt r (Ty t)
      | _ -> mismatch ())
  | Format_subst (_, s, r) -> (
      match ty with
      | Ty (Format_subst_ty (s', _, t)) ->
          same s s';
          type_fmt r (Ty t)
      | _ -> mismatch ())
  | Alpha r -> type_fmt r (expect 8 ty)
  | Theta r -> type_fmt r (expect 9 ty)
  | Formatting_gen (Open_tag (Format (f, _)), r) -> type_fmt r (type_fmt f ty)
  | Formatting_gen (Open_box (Format (f, _)), r) -> type_fmt r (type_fmt f ty)
  | Reader r -> type_fmt r (expect 11 ty)
  | Scan_char_set (_, _, r) -> type_fmt r (expect 1 ty)
  | Scan_get_counter (_, r) -> type_fmt r (expect 2 ty)
  | Ignored_param (Ignored_format_subst (_, s), r) -> type_subst (Ty s) r ty
  | Ignored_param (Ignored_reader, r) -> type_fmt r (expect 12 ty)
  | Ignored_param (_, r) -> type_fmt r ty
  | End_of_format -> ty
  | Scan_next_char _ | Custom _ -> mismatch ()

and type_subst : type a b c d e f. ty -> (a, b, c, d, e, f) fmt -> ty -> ty =
 fun (Ty sub) r ty ->
  match sub with
  | End_of_fmtty -> type_fmt r ty
  | Format_arg_ty (a, s) -> (
      match ty with
      | Ty (Format_arg_ty (b, t)) ->
          same a b;
          type_subst (Ty s) r (Ty t)
      | _ -> mismatch ())
  | Format_subst_ty (a, a', s) -> (
      match ty with
      | Ty (Format_subst_ty (b, b', t)) ->
          same a b;
          same a' b';
          type_subst (Ty s) r (Ty t)
      | _ -> mismatch ())
  | Any_ty _ -> mismatch ()
  | _ ->
      let k, s = step sub in
      type_subst s r (expect k ty)

(* [CamlinternalFormat.recast] calls [type_format]: it raises Type_mismatch
   where [type_fmt] does, and otherwise returns a format with the same
   printing behaviour as its argument. [type_fmt] compares sub-format types
   with [string_of_fmtty], which ignores the second witness of
   [Format_subst_ty]; the type checker builds it equal to the first
   (camlinternalFormatBasics.ml, NOTE [1]). *)
let recast :
    type a b c d e f g h i j k l.
    (a, b, c, d, e, f) fmt ->
    (a, b, c, d, e, f, g, h, i, j, k, l) fmtty_rel ->
    (g, h, i, j, k, l) fmt =
 fun fmt ty ->
  (match type_fmt fmt (Ty ty) with Ty End_of_fmtty -> () | _ -> mismatch ());
  Obj.magic fmt

let pad : type x y. (x, y) padding -> (padty -> int -> y) -> x =
 fun pad k ->
  match pad with
  | No_padding -> k Right 0
  | Lit_padding (padty, w) -> k padty w
  | Arg_padding padty -> fun w -> k padty w

let prec : type y z. (y, z) precision -> int -> (int -> z) -> y =
 fun prec default k ->
  match prec with
  | No_precision -> k default
  | Lit_precision p -> k p
  | Arg_precision -> fun p -> k p

let rec make_printf :
    type a b c d e f. ((b, c) acc -> f) -> (b, c) acc -> (a, b, c, d, e, f) fmt -> a
    =
 fun k acc fmt ->
  match fmt with
  | Char rest -> fun c -> make_printf k (Str (acc, chr c)) rest
  | Caml_char rest ->
      fun c -> make_printf k (Str (acc, "'" ^ escape '\'' (chr c) ^ "'")) rest
  | String (p, rest) -> make_padding k acc rest p (fun s -> s)
  | Caml_string (p, rest) ->
      make_padding k acc rest p (fun s -> "\"" ^ escape '"' s ^ "\"")
  | Int (iconv, p, q, rest) -> make_int k acc rest p q (convert_int iconv)
  | Int32 (iconv, p, q, rest) -> make_int k acc rest p q (convert_int32 iconv)
  | Nativeint (iconv, p, q, rest) ->
      make_int k acc rest p q (convert_nativeint iconv)
  | Int64 (iconv, p, q, rest) -> make_int k acc rest p q (convert_int64 iconv)
  | Float (fconv, p, q, rest) ->
      let default =
        match snd fconv with
        | Float_F -> 12
        | Float_f | Float_e | Float_E | Float_g | Float_G | Float_h | Float_H
        | Float_CF ->
            -6
      in
      pad p (fun padty w ->
          prec q default (fun q x ->
              make_printf k
                (Str (acc, fix_padding padty w (convert_float fconv q x)))
                rest))
  | Bool (p, rest) -> make_padding k acc rest p (fun b -> if b then "true" else "false")
  | Alpha rest -> fun f x -> make_printf k (Delay (acc, fun o -> f o x)) rest
  | Theta rest -> fun f -> make_printf k (Delay (acc, f)) rest
  | Custom (arity, f, rest) -> make_custom k acc rest arity (f ())
  | Reader _ -> impossible ()
  | Flush rest -> make_printf k (Flush acc) rest
  | String_literal (s, rest) -> make_printf k (Str (acc, s)) rest
  | Char_literal (c, rest) -> make_printf k (Str (acc, chr c)) rest
  | Format_arg (_, ty, rest) ->
      let ty = string_of_fmtty ty in
      fun _ -> make_printf k (Str (acc, ty)) rest
  | Format_subst (_, ty, rest) ->
      fun (Format (fmt, _)) ->
        make_printf (fun acc -> make_printf k acc rest) acc (recast fmt ty)
  | Scan_char_set (_, _, rest) ->
      let acc = Invalid (acc, "Printf: bad conversion %[") in
      fun _ -> make_printf k acc rest
  | Scan_get_counter (_, rest) ->
      fun n -> make_printf k (Str (acc, convert_int Int_u n)) rest
  | Scan_next_char rest -> fun c -> make_printf k (Str (acc, chr c)) rest
  | Ignored_param (ign, rest) -> make_ignored_param k acc ign rest
  | Formatting_lit (lit, rest) ->
      make_printf k (Str (acc, string_of_formatting_lit lit)) rest
  | Formatting_gen (Open_tag (Format (fmt, _)), rest) ->
      make_printf (fun acc -> make_printf k acc rest) (Str (acc, "@{")) fmt
  | Formatting_gen (Open_box (Format (fmt, _)), rest) ->
      make_printf (fun acc -> make_printf k acc rest) (Str (acc, "@[")) fmt
  | End_of_format -> k acc

and make_padding :
    type x z a b c d e f.
    ((b, c) acc -> f) ->
    (b, c) acc ->
    (a, b, c, d, e, f) fmt ->
    (x, z -> a) padding ->
    (z -> string) ->
    x =
 fun k acc fmt p trans ->
  pad p (fun padty w x ->
      make_printf k (Str (acc, fix_padding padty w (trans x))) fmt)

and make_int :
    type x y z a b c d e f.
    ((b, c) acc -> f) ->
    (b, c) acc ->
    (a, b, c, d, e, f) fmt ->
    (x, y) padding ->
    (y, z -> a) precision ->
    (z -> string) ->
    x =
 fun k acc fmt p q trans ->
  pad p (fun padty w ->
      prec q 0 (fun q x ->
          make_printf k
            (Str (acc, fix_padding padty w (fix_int_precision q (trans x))))
            fmt))

and make_custom :
    type x y a b c d e f.
    ((b, c) acc -> f) ->
    (b, c) acc ->
    (a, b, c, d, e, f) fmt ->
    (a, x, y) custom_arity ->
    x ->
    y =
 fun k acc rest arity f ->
  match arity with
  | Custom_zero -> make_printf k (Str (acc, f)) rest
  | Custom_succ arity -> fun x -> make_custom k acc rest arity (f x)

and make_ignored_param :
    type x y a b c d e f.
    ((b, c) acc -> f) ->
    (b, c) acc ->
    (a, b, c, d, y, x) ignored ->
    (x, b, c, y, e, f) fmt ->
    a =
 fun k acc ign fmt ->
  match ign with
  | Ignored_format_subst (_, ty) ->
skip_fmtty (fun () -> make_invalid_arg k acc fmt) ty
  | Ignored_reader -> impossible ()
  | Ignored_char -> make_invalid_arg k acc fmt
  | Ignored_caml_char -> make_invalid_arg k acc fmt
  | Ignored_string _ -> make_invalid_arg k acc fmt
  | Ignored_caml_string _ -> make_invalid_arg k acc fmt
  | Ignored_int _ -> make_invalid_arg k acc fmt
  | Ignored_int32 _ -> make_invalid_arg k acc fmt
  | Ignored_nativeint _ -> make_invalid_arg k acc fmt
  | Ignored_int64 _ -> make_invalid_arg k acc fmt
  | Ignored_float _ -> make_invalid_arg k acc fmt
  | Ignored_bool _ -> make_invalid_arg k acc fmt
  | Ignored_format_arg _ -> make_invalid_arg k acc fmt
  | Ignored_scan_char_set _ -> make_invalid_arg k acc fmt
  | Ignored_scan_get_counter _ -> make_invalid_arg k acc fmt
  | Ignored_scan_next_char -> make_invalid_arg k acc fmt

and make_invalid_arg :
    type a b c d e f. ((b, c) acc -> f) -> (b, c) acc -> (a, b, c, d, e, f) fmt -> a
    =
 fun k acc fmt -> make_printf k (Invalid (acc, "Printf: bad conversion %_")) fmt

and skip_fmtty :
    type a1 b1 c1 d1 e1 f1 a2 b2 c2 d2 e2 f2.
    (unit -> f2) -> (a1, b1, c1, d1, e1, f1, a2, b2, c2, d2, e2, f2) fmtty_rel -> a2 =
 fun k ty ->
  match ty with
  | Char_ty r -> fun _ -> skip_fmtty k r
  | String_ty r -> fun _ -> skip_fmtty k r
  | Int_ty r -> fun _ -> skip_fmtty k r
  | Int32_ty r -> fun _ -> skip_fmtty k r
  | Nativeint_ty r -> fun _ -> skip_fmtty k r
  | Int64_ty r -> fun _ -> skip_fmtty k r
  | Float_ty r -> fun _ -> skip_fmtty k r
  | Bool_ty r -> fun _ -> skip_fmtty k r
  | Alpha_ty r -> fun _ _ -> skip_fmtty k r
  | Theta_ty r -> fun _ -> skip_fmtty k r
  | Any_ty r -> fun _ -> skip_fmtty k r
  | Format_arg_ty (_, r) -> fun _ -> skip_fmtty k r
  | Format_subst_ty (_, ty, r) ->
      fun _ -> skip_fmtty (fun () -> skip_fmtty k r) ty
  | Reader_ty _ -> impossible ()
  | Ignored_reader_ty _ -> impossible ()
  | End_of_fmtty -> k ()

let const x _ = x

let skip_pad : type x y. (x, y) padding -> y -> x =
 fun p r -> match p with Arg_padding _ -> const r | No_padding -> r | Lit_padding _ -> r

let skip_prec : type y z. (y, z) precision -> z -> y =
 fun q r -> match q with Arg_precision -> const r | No_precision -> r | Lit_precision _ -> r

let rec make_iprintf : type a b c d e f s. (s -> f) -> s -> (a, b, c, d, e, f) fmt -> a =
 fun k o fmt ->
  match fmt with
  | Char rest -> const (make_iprintf k o rest)
  | Caml_char rest -> const (make_iprintf k o rest)
  | String (p, rest) -> skip_pad p (const (make_iprintf k o rest))
  | Caml_string (p, rest) -> skip_pad p (const (make_iprintf k o rest))
  | Int (_, p, q, rest) -> skip_pad p (skip_prec q (const (make_iprintf k o rest)))
  | Int32 (_, p, q, rest) ->
      skip_pad p (skip_prec q (const (make_iprintf k o rest)))
  | Nativeint (_, p, q, rest) ->
      skip_pad p (skip_prec q (const (make_iprintf k o rest)))
  | Int64 (_, p, q, rest) ->
      skip_pad p (skip_prec q (const (make_iprintf k o rest)))
  | Float (_, p, q, rest) ->
      skip_pad p (skip_prec q (const (make_iprintf k o rest)))
  | Bool (p, rest) -> skip_pad p (const (make_iprintf k o rest))
  | Alpha rest -> const (const (make_iprintf k o rest))
  | Theta rest -> const (make_iprintf k o rest)
  | Custom (arity, _, rest) -> skip_custom k o rest arity
  | Reader _ -> impossible ()
  | Flush rest -> make_iprintf k o rest
  | String_literal (_, rest) -> make_iprintf k o rest
  | Char_literal (_, rest) -> make_iprintf k o rest
  | Format_arg (_, _, rest) -> const (make_iprintf k o rest)
  | Format_subst (_, ty, rest) ->
      fun (Format (fmt, _)) ->
        make_iprintf (fun o -> make_iprintf k o rest) o (recast fmt ty)
  | Scan_char_set (_, _, rest) -> const (make_iprintf k o rest)
  | Scan_get_counter (_, rest) -> const (make_iprintf k o rest)
  | Scan_next_char rest -> const (make_iprintf k o rest)
  | Ignored_param (ign, rest) -> make_ignored_param (fun _ -> k o) End ign rest
  | Formatting_lit (_, rest) -> make_iprintf k o rest
  | Formatting_gen (Open_tag (Format (fmt, _)), rest) ->
      make_iprintf (fun o -> make_iprintf k o rest) o fmt
  | Formatting_gen (Open_box (Format (fmt, _)), rest) ->
      make_iprintf (fun o -> make_iprintf k o rest) o fmt
  | End_of_format -> k o

and skip_custom :
    type x y a b c d e f s.
    (s -> f) -> s -> (a, b, c, d, e, f) fmt -> (a, x, y) custom_arity -> y =
 fun k o fmt arity ->
  match arity with
  | Custom_zero -> make_iprintf k o fmt
  | Custom_succ arity -> const (skip_custom k o fmt arity)

let rec output_acc o = function
  | End -> ()
  | Str (p, s) ->
      output_acc o p;
      output_string o s
  | Delay (p, f) ->
      output_acc o p;
      f o
  | Flush p ->
      output_acc o p;
      flush o
  | Invalid (p, msg) ->
      output_acc o p;
      raise (Invalid_argument msg)

let rec bufput_acc b = function
  | End -> ()
  | Str (p, s) ->
      bufput_acc b p;
      Buffer.add_string b s
  | Delay (p, f) ->
      bufput_acc b p;
      f b
  | Flush p -> bufput_acc b p
  | Invalid (p, msg) ->
      bufput_acc b p;
      raise (Invalid_argument msg)

let rec strput_acc = function
  | End -> ""
  | Str (p, s) ->
      let r = strput_acc p in
      r ^ s
  | Delay (p, f) ->
      let r = strput_acc p in
      r ^ f ()
  | Flush p -> strput_acc p
  | Invalid (p, msg) ->
      let _ = strput_acc p in
      raise (Invalid_argument msg)

let kfprintf k o (Format (fmt, _)) =
  make_printf
    (fun acc ->
      output_acc o acc;
      k o)
    End fmt

let kbprintf k b (Format (fmt, _)) =
  make_printf
    (fun acc ->
      bufput_acc b acc;
      k b)
    End fmt

let ikfprintf k oc (Format (fmt, _)) = make_iprintf k oc fmt
let ikbprintf = ikfprintf
let fprintf oc fmt = kfprintf ignore oc fmt
let bprintf b fmt = kbprintf ignore b fmt
let ifprintf oc fmt = ikfprintf ignore oc fmt
let ibprintf b fmt = ikbprintf ignore b fmt
let printf fmt = fprintf stdout fmt
let eprintf fmt = fprintf stderr fmt

let ksprintf k (Format (fmt, _)) =
  make_printf (fun acc -> k (strput_acc acc)) End fmt

let sprintf fmt = ksprintf (fun s -> s) fmt
let kprintf = ksprintf

module Args = Stdlib.Printf.Args

let rec apply : type a r. a -> (a, r) Args.t -> r =
 fun f -> function [] -> f | x :: rest -> apply (f x) rest

let lfprintf oc (Format (fmt, _)) args =
  apply (make_printf (output_acc oc) End fmt) args

let lbprintf b (Format (fmt, _)) args =
  apply (make_printf (bufput_acc b) End fmt) args

let lprintf fmt args = lfprintf stdout fmt args
let leprintf fmt args = lfprintf stderr fmt args

let lsprintf (Format (fmt, _)) args =
  apply (make_printf strput_acc End fmt) args
