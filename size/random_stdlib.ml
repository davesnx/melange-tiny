module R = Random

let () =
  R.init 42;
  for _ = 1 to 8 do
    Js.log (R.bits ())
  done;
  R.init (-7);
  Js.log3 (R.int 1000) (R.float 1.) (R.bool ());
  R.self_init ();
  Js.log3 (R.int 10) (R.float 1.) (R.bool ())
