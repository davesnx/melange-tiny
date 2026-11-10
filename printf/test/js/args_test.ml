module P = Tiny_printf.Printf

let checks = ref 0
let failures = ref 0

let run f =
  match f () with
  | s -> "ok " ^ String.escaped s
  | exception e -> "exception " ^ Printexc.to_string e

let check name expected actual =
  incr checks;
  let e = run expected and a = run actual in
  if e <> a then begin
    incr failures;
    print_endline (name ^ "\n  stdlib: " ^ e ^ "\n  tiny:   " ^ a)
  end

let mark s = print_string ("\001" ^ s ^ "\001")

let chan name f g =
  let go f =
    let r = run (fun () -> f (); "") in
    flush stdout;
    flush stderr;
    r
  in
  mark ("REF " ^ name);
  let e = go f in
  mark ("TINY " ^ name);
  let a = go g in
  mark "END";
  incr checks;
  if e <> a then begin
    incr failures;
    print_endline (name ^ "\n  stdlib: " ^ e ^ "\n  tiny:   " ^ a)
  end

let pa () x = "<" ^ x ^ ">"
let ba b x = Buffer.add_string b ("<" ^ x ^ ">")
let oa o x = output_string o ("<" ^ x ^ ">")

let () =
  check "lsprintf"
    (fun () -> Printf.lsprintf "%s %d %.2f %c%a" [ "ocaml"; 42; 3.14; 'c'; pa; "x" ])
    (fun () -> P.lsprintf "%s %d %.2f %c%a" [ "ocaml"; 42; 3.14; 'c'; pa; "x" ]);
  check "lsprintf invalid"
    (fun () -> Printf.lsprintf "%s%_d" [ "x" ])
    (fun () -> P.lsprintf "%s%_d" [ "x" ]);
  check "lbprintf"
    (fun () ->
      let b = Buffer.create 1 in
      Printf.lbprintf b "%d%a%!" [ 1; ba; "y" ];
      Buffer.contents b)
    (fun () ->
      let b = Buffer.create 1 in
      P.lbprintf b "%d%a%!" [ 1; ba; "y" ];
      Buffer.contents b);
  check "Args.apply"
    (fun () -> Printf.Args.apply (Printf.sprintf "%d-%s") [ 1; "a" ])
    (fun () -> P.Args.apply (P.sprintf "%d-%s") [ 1; "a" ]);
  check "Args.@"
    (fun () -> Printf.lsprintf "%d%s%c" Printf.Args.([ 1 ] @ [ "a"; 'c' ]))
    (fun () -> P.lsprintf "%d%s%c" P.Args.([ 1 ] @ [ "a"; 'c' ]));
  let same : ('a, 'r) Printf.Args.t -> ('a, 'r) P.Args.t = fun x -> x in
  ignore (same [ 1 ]);
  chan "lprintf" (fun () -> Printf.lprintf "a%s\n%a" [ "x"; oa; "y" ])
    (fun () -> P.lprintf "a%s\n%a" [ "x"; oa; "y" ]);
  chan "leprintf" (fun () -> Printf.leprintf "a%s\nb%!c" [ "x" ])
    (fun () -> P.leprintf "a%s\nb%!c" [ "x" ]);
  chan "lfprintf" (fun () -> Printf.lfprintf stderr "@[%s@ %d@]@.\n" [ "x ="; 1 ])
    (fun () -> P.lfprintf stderr "@[%s@ %d@]@.\n" [ "x ="; 1 ]);
  print_endline
    (string_of_int !checks ^ " Melange-only checks, " ^ string_of_int !failures
   ^ " failures");
  if !failures > 0 then exit 1
