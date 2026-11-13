let native = true
let describe = Printexc.to_string
let wide_strings = []
let install_fake_runtime () = ()
let counter = ref 0

let prepare contents =
  incr counter;
  let path = Filename.temp_file (Printf.sprintf "buffertest%d" !counter) ".bin" in
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc;
  path

let read_back path = In_channel.with_open_bin path In_channel.input_all
