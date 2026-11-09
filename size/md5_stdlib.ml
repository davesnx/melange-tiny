let () =
  let s = Js.Dict.unsafeGet [%mel.raw "globalThis"] "input" in
  Js.log (Digest.to_hex (Digest.string s))
