let () =
  let s = Js.Dict.unsafeGet [%mel.raw "globalThis"] "input" in
  Js.log (Tiny_md5.Digest.to_hex (Tiny_md5.Digest.string s))
