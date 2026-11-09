module I = Stdlib.Int64

let () =
  let a = I.of_string "1700000000000" in
  let b = I.add (I.mul a (I.of_int 1000)) I.one in
  let c = I.shift_right_logical (I.div b (I.of_int 7)) 3 in
  Js.log (I.to_string c);
  Js.log (string_of_float (I.to_float (I.sub c a)));
  Js.log (string_of_int (I.compare a b))
