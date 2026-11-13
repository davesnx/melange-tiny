let native = false

let describe = function
  | Js.Exn.Error e -> "JS Error: " ^ (match Js.Exn.message e with Some m -> m | None -> "")
  | e -> Printexc.to_string e

external from_code : int -> string = "fromCharCode" [@@mel.scope "String"]

let wide_strings =
  let w = from_code in
  [ w 256; w 0x20ac ^ "a" ^ w 0xffff; w 0xd83d ^ w 0xde00; w 0xdc00 ^ "$x" ^ w 0xd800;
    String.concat "" (List.init 40 (fun i -> w ((i * 7919) land 0xffff))) ]

let install_fake_runtime : unit -> unit =
  [%mel.raw
    {|function () {
  var store = {};
  globalThis.__buffer_store = store;
  globalThis.caml_sys_open = function (name, mode, perm) { return name; };
  globalThis.caml_ml_open_descriptor_in = function (fd) {
    return { fd: fd, data: store[fd], pos: 0, calls: 0 };
  };
  globalThis.caml_ml_open_descriptor_out = function (fd) {
    store[fd] = "";
    return { fd: fd, buffer: "", output: function (_, s) { store[fd] += s; } };
  };
  globalThis.caml_ml_set_channel_name = function (c, name) { return 0; };
  globalThis.caml_ml_close_channel = function (c) { return 0; };
  globalThis.caml_ml_input = function (ic, buf, ofs, len) {
    if (ic.data.startsWith("boom") && ic.calls++ > 0) throw new Error("input failed");
    var n = Math.min(len, ic.data.length - ic.pos, 3);
    for (var i = 0; i < n; i++) buf[ofs + i] = ic.data.charCodeAt(ic.pos + i);
    ic.pos += n;
    return n;
  };
  globalThis.caml_ml_output_bytes = function (oc, buf, ofs, len) {
    store[oc.fd] += "[" + Array.from(buf.slice(ofs, ofs + len)).join(",") + "]";
  };
}|}]

let counter = ref 0

let set_file : string -> string -> unit =
  [%mel.raw {|function (name, contents) { globalThis.__buffer_store[name] = contents; }|}]

let read_back : string -> string =
  [%mel.raw {|function (name) { return globalThis.__buffer_store[name]; }|}]

let prepare contents =
  incr counter;
  let path = "fake" ^ string_of_int !counter in
  set_file path contents;
  path
