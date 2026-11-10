let native = false

let describe = function
  | Js.Exn.Error e ->
      "JS Error: "
      ^ (match Js.Exn.message e with Some m -> m | None -> "")
  | e -> Printexc.to_string e

external from_code : int -> string = "fromCharCode" [@@mel.scope "String"]

let wide_strings =
  let w = from_code in
  [ w 256; w 0x20ac; w 0xffff; w 2703; w 2704; w 2703 ^ w 2704;
    String.make 63 'a' ^ w 0x1234;
    w 0x1ff ^ String.make 63 'b';
    String.concat "" (List.init 64 (fun i -> w (i * 1031 land 0xffff)));
    String.concat "" (List.init 200 (fun i -> w ((i * 7919) land 0xffff))) ]

let install_fake_runtime : unit -> unit =
  [%mel.raw
    {|function () {
  var store = {};
  globalThis.__md5_store = store;
  globalThis.caml_sys_open = function (name, mode, perm) {
    if (!(name in store)) throw new Error("no such file: " + name);
    return name;
  };
  globalThis.caml_ml_open_descriptor_in = function (fd) {
    return { fd: fd, data: store[fd], pos: 0, closed: false };
  };
  globalThis.caml_ml_open_descriptor_out = function (fd) {
    store[fd] = "";
    return { fd: fd, buffer: "", output: function (_, s) { store[fd] += s; } };
  };
  globalThis.caml_ml_set_channel_name = function (c, name) { return 0; };
  globalThis.__md5_closes = 0;
  globalThis.caml_ml_close_channel = function (c) {
    if (c.closed) throw new Error("closed twice");
    c.closed = true;
    globalThis.__md5_closes++;
    return 0;
  };
  globalThis.caml_ml_input = function (ic, buf, ofs, len) {
    if (ic.data === "boom") throw new Error("input failed");
    var n = Math.min(len, ic.data.length - ic.pos, 1000);
    for (var i = 0; i < n; i++) buf[ofs + i] = ic.data.charCodeAt(ic.pos + i);
    ic.pos += n;
    return n;
  };
  globalThis.caml_md5_chan = function (ic, n) {
    if (ic.data === "boom") throw new Error("md5_chan failed");
    return "md5chan|" + n + "|" + ic.data.slice(ic.pos);
  };
  globalThis.caml_blake2_create = function (len, key) {
    return { len: len, key: key, log: [] };
  };
  globalThis.caml_blake2_update = function (st, buf, ofs, n) {
    st.log.push(ofs + ":" + n + ":" + Array.from(buf.slice(ofs, ofs + n)).join(","));
  };
  globalThis.caml_blake2_final = function (st, len) {
    return "final|" + st.len + "|" + st.key + "|" + len + "|" + st.log.join(";");
  };
  globalThis.caml_blake2_string = function (len, key, s, ofs, n) {
    return "string|" + len + "|" + key + "|" + s + "|" + ofs + "|" + n;
  };
  globalThis.caml_blake2_bytes = function (len, key, b, ofs, n) {
    return "bytes|" + len + "|" + key + "|" + Array.from(b).join(",") + "|" + ofs + "|" + n;
  };
}|}]


let counter = ref 0

let set_file : string -> string -> unit =
  [%mel.raw {|function (name, contents) { globalThis.__md5_store[name] = contents; }|}]

let get_file : string -> string =
  [%mel.raw {|function (name) { return globalThis.__md5_store[name]; }|}]

let prepare contents =
  incr counter;
  let path = "fake" ^ string_of_int !counter in
  set_file path contents;
  path

let read_back path = get_file path

let closes : unit -> int = [%mel.raw {|function () { return globalThis.__md5_closes; }|}]

let restore_backtrace : unit -> unit =
  [%mel.raw {|function () { globalThis.caml_restore_raw_backtrace = function (e, bt) { return 0; }; }|}]
