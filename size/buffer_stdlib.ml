module B = Buffer

let () =
  let get k = Js.Dict.unsafeGet [%mel.raw "globalThis"] k in
  let s : string = get "input" and c : char = get "char" in
  let b = B.create 16 in
  B.add_string b s;
  B.add_char b c;
  B.add_substring b s 1 2;
  Js.log (B.contents b)
