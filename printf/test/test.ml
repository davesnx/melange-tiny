open CamlinternalFormatBasics
module P = Tiny_printf.Printf

module _ : module type of struct
  include Stdlib.Printf
end =
  P

module _ : module type of struct
  include P
end =
  Stdlib.Printf

let js = match Sys.backend_type with Sys.Other _ -> true | _ -> false
let checks = ref 0
let failures = ref 0
let log = ref []
let note s = log := s :: !log

let run f =
  log := [];
  let r =
    match f () with
    | s -> "ok " ^ String.escaped s
    | exception e -> "exception " ^ Printexc.to_string e
  in
  r ^ " log [" ^ String.concat "; " (List.rev !log) ^ "]"

let fail name expected actual =
  incr failures;
  if !failures <= 200 then
    print_endline (name ^ "\n  stdlib: " ^ expected ^ "\n  tiny:   " ^ actual)

let check name expected actual =
  incr checks;
  let e = run expected in
  let a = run actual in
  if e <> a then fail name e a

let fmt_name fmt = String.escaped (string_of_format fmt)

let each fmts values show f =
  List.iter
    (fun fmt ->
      List.iter
        (fun v -> f (fmt_name fmt ^ " " ^ show v) fmt v)
        values)
    fmts

let test1 fmts values show =
  each fmts values show (fun name fmt v ->
      check name (fun () -> Printf.sprintf fmt v) (fun () -> P.sprintf fmt v))

let test2 fmts values show =
  each fmts values show (fun name fmt (a, b) ->
      check name
        (fun () -> Printf.sprintf fmt a b)
        (fun () -> P.sprintf fmt a b))

let test3 fmts values show =
  each fmts values show (fun name fmt (a, b, c) ->
      check name
        (fun () -> Printf.sprintf fmt a b c)
        (fun () -> P.sprintf fmt a b c))

let seed = ref 0x2545F4914F6CDD1DL

let next () =
  seed := Int64.add (Int64.mul !seed 6364136223846793005L) 1442695040888963407L;
  Int64.logxor !seed (Int64.shift_right_logical !seed 29)

let rand n = Int64.to_int (Int64.unsigned_rem (next ()) (Int64.of_int n))

let ints =
  [ 0; 1; -1; 2; 7; 8; 9; 10; 15; 16; 17; 31; 32; 99; 100; 127; 128; 255;
    256; 999; 1000; 1001; 4095; 4096; 65535; 65536; 999_999; 1_000_000;
    16_777_215; 123_456_789; -123_456_789; 0x7fff_ffff; -0x7fff_ffff;
    -0x8000_0000; -2; -15; -16; -255; -256; -1000; -65536; max_int; min_int;
    max_int - 1; min_int + 1; 1 lsl 30; 1 lsl 31; 1 lsl 32; 1 lsl 40;
    -(1 lsl 40); max_int / 3; min_int / 7 ]
  @ List.init 300 (fun _ -> Int64.to_int (next ()))
  @ List.init 100 (fun _ -> rand 100_000 - 50_000)

let int32s =
  [ 0l; 1l; -1l; 7l; 10l; 255l; -255l; 65536l; 123456789l; -123456789l;
    Int32.max_int; Int32.min_int; Int32.add Int32.min_int 1l;
    Int32.sub Int32.max_int 1l ]
  @ List.init 100 (fun _ -> Int64.to_int32 (next ()))

let nativeints =
  List.map
    (fun x -> Int64.to_nativeint (Sys.opaque_identity x))
    [ 0L; 1L; -1L; 7L; 10L; 255L; -255L; 123456789L; -123456789L;
      2147483647L; -2147483648L; -2147483647L; 4294967296L ]
  @ List.init 100 (fun _ -> Int64.to_nativeint (next ()))

let int64s =
  [ 0L; 1L; -1L; 7L; 8L; 10L; 15L; 16L; 255L; -255L; 1000L; 65535L;
    0xffff_ffffL; 0x1_0000_0000L; -0x1_0000_0000L; 0x1_0000_0001L;
    -0xffff_ffffL; 4294967295L; 4294967296L; 9007199254740992L;
    9007199254740993L; -9007199254740993L; 123456789012345678L;
    -123456789012345678L; 999999999999999999L; 1000000000000000000L;
    Int64.max_int; Int64.min_int; Int64.add Int64.min_int 1L;
    Int64.sub Int64.max_int 1L; Int64.shift_left 1L 63; Int64.shift_left 1L 62;
    Int64.shift_left 1L 32; Int64.shift_left 1L 31 ]
  @ List.init 300 (fun _ -> next ())
  @ List.init 100 (fun _ -> Int64.shift_right (next ()) (rand 64))

let floats =
  [ 0.; -0.; 1.; -1.; 2.; 10.; 100.; 0.1; 0.2; 0.3; 0.5; 1.5; 2.5; -2.5; 3.5;
    0.125; 0.0625; 1. /. 3.; 2. /. 3.; -1. /. 3.; 3.14159265358979;
    2.718281828459045; 1e-5; 1e-4; 0.000123456; 1e-7; 1.5e-10; 123456.;
    1234567.; 999999.5; 9.9999999; 99.99; 0.95; 0.05; 0.005; 0.0005; 1e15;
    1e16; 1e17; 1e20; 1e21; 1e22; 1e23; 1e100; -1e100; 1e300; 1e-300; 5e-324;
    -5e-324; 1e-310; 2.2250738585072014e-308; 2.225073858507201e-308;
    max_float; -.max_float; min_float; epsilon_float; 4503599627370496.;
    4503599627370495.5; 9007199254740993.; 0.1 +. 0.2; 123.456; -0.001;
    1073741824.; 2147483648.; 4294967296.; 1e-6; 9.5; 0.95e-5; 12345.678;
    5.5e-5; 6.02214076e23; 1.7976931348623157e308; 1e-320; 0.999999;
    0.9999995; 99999.95; 9999999.; 1e6; 1e7; 100000.; 999999.;
    nan; -.nan; infinity; neg_infinity ]
  @ List.init 300 (fun _ -> Int64.float_of_bits (next ()))
  @ List.init 300 (fun i ->
        let m = Int64.to_float (Int64.shift_right_logical (next ()) 11) in
        let e = rand 70 - 35 in
        (if i mod 3 = 0 then -1. else 1.) *. m /. 9007199254740992.
        *. (10. ** float_of_int e))
  @ List.init 100 (fun _ ->
        float_of_int (rand 2_000_001 - 1_000_000) /. float_of_int (1 + rand 1000))

let all_chars = String.init 256 Char.chr

let strings =
  [ ""; "a"; "ab"; "hello world"; "with \"quotes\""; "it's"; "tab\there";
    "nl\n"; "cr\r"; "bs\b"; "\\"; "\000"; "\127"; "\128\255"; "%d"; "@[";
    "caf\195\169"; "a longer string that is wider than any width";
    all_chars ]
  @ List.init 256 (fun i -> String.make 1 (Char.chr i))
  @ List.init 50 (fun _ -> String.init (rand 12) (fun _ -> Char.chr (rand 256)))

let chars = List.init 256 Char.chr
let widths = [ -20; -5; -1; 0; 1; 2; 5; 8; 20; 30; min_int ]
let precisions = [ -20; -3; -1; 0; 1; 2; 3; 6; 10; 17; 20; min_int ]

let fprecisions =
  [ -20; -12; -6; -3; -1; 0; 1; 2; 3; 5; 6; 10; 12; 13; 15; 16; 17; 18; 20;
    21; 22; 25; 30; 50; 99; 100; 101; 102; 200; min_int ]

let pairs a b = List.concat_map (fun x -> List.map (fun y -> (x, y)) b) a

let triples a b c =
  List.concat_map (fun (x, y) -> List.map (fun z -> (x, y, z)) c) (pairs a b)

let some l = List.filteri (fun i _ -> i mod 7 = 0) l
let si = string_of_int
let sf = Printf.sprintf "%h"
let ss = String.escaped
let sc c = si (Char.code c)
let s2 f g (a, b) = f a ^ " " ^ g b
let s3 f g h (a, b, c) = f a ^ " " ^ g b ^ " " ^ h c

let () =
  test1
    [ "%d"; "%i"; "%u"; "%x"; "%X"; "%o"; "%#x"; "%#X"; "%#o"; "%#d"; "%#i";
      "%#u"; "%+d"; "% d"; "%+i"; "% i"; "%5d"; "%-5d"; "%05d"; "%+05d";
      "% 05d"; "%.3d"; "%5.3d"; "%-8.3x"; "%08x"; "%#08x"; "%#8x"; "%-#8x";
      "%#-8x"; "%#.5x"; "%+.5d"; "%.0d"; "%1d"; "%20d"; "%020d"; "%-20d";
      "%#20d"; "%#020d"; "%#-20d"; "%#.10d"; "%010u"; "%#5o"; "%#05o";
      "%.10o"; "%#.10o"; "%08X"; "%#08X"; "%-+8d"; "%- 8d"; "%+-8d"; "%#-15u";
      "%.12x"; "%#.12X"; "%12.8d"; "%-12.8i"; "%+5d"; "%0+5d"; "% 12.5i";
      "%#015d"; "%#15d"; "%#.3u"; "%30.25X"; "[%d]"; "a%db"; "%l"; "%n";
      "%L"; "%N"; "%_d%d"; "%d%_i"; "%_(%d%)" ]
    ints si;
  test1
    [ "%ld"; "%li"; "%lu"; "%lx"; "%lX"; "%lo"; "%#lx"; "%#lX"; "%#lo"; "%#ld";
      "%#lu"; "%+ld"; "% ld"; "%5ld"; "%-5ld"; "%05ld"; "%.3ld"; "%08lX";
      "%#012lx"; "%-#12lo"; "%+.12li"; "%20.15lu" ]
    int32s Int32.to_string;
  test1
    [ "%nd"; "%ni"; "%nu"; "%nx"; "%nX"; "%no"; "%#nx"; "%#nX"; "%#no";
      "%#nd"; "%#nu"; "%+nd"; "% nd"; "%08nx"; "%.4nd"; "%-15nd"; "%015nu" ]
    nativeints (Printf.sprintf "%nd");
  test1
    [ "%Ld"; "%Li"; "%Lu"; "%Lx"; "%LX"; "%Lo"; "%#Lx"; "%#LX"; "%#Lo";
      "%#Ld"; "%#Li"; "%#Lu"; "%+Ld"; "% Ld"; "%+Li"; "%25Ld"; "%-25Ld";
      "%025Ld"; "%.20Ld"; "%30.22Lx"; "%#30Lx"; "%#030Lx"; "%+025Li";
      "%.0Lu"; "%#-30Lo"; "%-30Lu"; "%.25Lo" ]
    int64s Int64.to_string;
  test1
    [ "%f"; "%e"; "%E"; "%g"; "%G"; "%h"; "%H"; "%F"; "%#F"; "%+f"; "% f";
      "%+e"; "% e"; "%+E"; "%+g"; "% g"; "%+G"; "%+F"; "% F"; "%+h"; "% h";
      "%+H"; "% H"; "%+#F"; "% #F"; "%.0f"; "%.1f"; "%.2f"; "%.3f"; "%.10f";
      "%.17f"; "%.20f"; "%.30f"; "%.0e"; "%.1e"; "%.3e"; "%.10e"; "%.16e";
      "%.20e"; "%.0E"; "%.0g"; "%.1g"; "%.2g"; "%.3g"; "%.6g"; "%.10g";
      "%.15g"; "%.16g"; "%.17g"; "%.20g"; "%.21g"; "%.22g"; "%.25g"; "%.0G";
      "%.12G"; "%.0h"; "%.1h"; "%.3h"; "%.12h"; "%.13h"; "%.14h"; "%.20h";
      "%.0H"; "%.5H"; "%.0F"; "%.1F"; "%.3F"; "%.12F"; "%.16F"; "%.17F";
      "%.20F"; "%#.3F"; "%#.0F"; "%#.20F"; "%10f"; "%-10f"; "%010f"; "%+010f";
      "% 010f"; "%010.3e"; "%-15.3e"; "%15.3g"; "%-15.3g"; "%015.3g";
      "%+015.3G"; "%20F"; "%-20F"; "%020F"; "%+020F"; "%20h"; "%020h";
      "%-20h"; "%020H"; "%+020h"; "%#20F"; "%#020F"; "%-#20F"; "%30.20f";
      "%#.3f"; "%#e"; "%#g"; "%#h"; "%5.0f"; "%.100f"; "%.100e"; "%.101f";
      "%.101e"; "%.101g"; "%.102g"; "%.100h"; "%+.0e"; "% .0f"; "%_f%f";
      "[%f]" ]
    floats sf;
  test1
    [ "%s"; "%5s"; "%-5s"; "%S"; "%10S"; "%-10S"; "%1s"; "%0s"; "[%s]";
      "%30s"; "%-30S"; "%_s%s"; "%[a-z]"; "%_[a-z]%s" ]
    strings ss;
  test1 [ "%c"; "%C"; "[%c]"; "%0c"; "%_c%c"; "%_C%C" ] chars sc;
  test1 [ "%B"; "%b"; "%10B"; "%-10B"; "%1b"; "%_B%B" ] [ true; false ]
    string_of_bool;
  List.iter
    (fun (fmt : (string, unit, string) format) ->
      check (fmt_name fmt) (fun () -> Printf.sprintf fmt) (fun () -> P.sprintf fmt))
    [ ""; "abc"; "%%"; "%!"; "%@"; "%,"; "a%!b%%c"; "@"; "@@"; "%%%%";
      "100%%"; "@[<hov 2>a@ b@,c@;<1 2>d@]@.@{<t>x@}@?@\n@<5>z@@ @%%";
      "@[@]@{@}"; "@;"; "@;<3 4>"; "@<3>"; "@z"; "@\n@."; "@[<v>"; "@{<a b>";
      "%_d"; "%_s"; "%_{%d%}"; "%!%!"; "x%_cy"; "%_[a-z]"; "%_0c"; "%_l";
      "%_Ld"; "%_ld"; "%_nd"; "%_S"; "%_C"; "%_B"; "%_5d"; "%_5f"; "%_.3f" ];
  test2
    [ "%*d"; "%-*d"; "%0*d"; "%.*d"; "%+.*d"; "%*x"; "%#*x"; "%0*X"; "%-*u";
      "%.*o"; "%#.*x"; "%*i"; "%#0*d"; "%#*d"; "%5.*d"; "%*.3d"; "%-*.5x" ]
    (pairs widths (some ints @ [ 0; -1; max_int; min_int ]))
    (s2 si si);
  test3
    [ "%*.*d"; "%-*.*x"; "%+*.*d"; "%#*.*o"; "%*.*u"; "%#*.*d" ]
    (triples widths precisions [ 0; 1; -1; 123; -12345; max_int; min_int ])
    (s3 si si si);
  test2 [ "%*ld"; "%.*lx"; "%-*ld" ]
    (pairs precisions [ 0l; -1l; Int32.max_int; Int32.min_int ])
    (s2 si Int32.to_string);
  test2 [ "%0*nd"; "%.*nu"; "%#*nx" ]
    (pairs precisions (List.filteri (fun i _ -> i < 4 || i = 9 || i = 10) nativeints))
    (s2 si (Printf.sprintf "%nd"));
  test2 [ "%*Ld"; "%.*Lx"; "%0*Lu"; "%#.*Lo"; "%+.*Ld" ]
    (pairs precisions (some int64s @ [ Int64.min_int; Int64.max_int ]))
    (s2 si Int64.to_string);
  test2
    [ "%.*f"; "%.*e"; "%.*E"; "%.*g"; "%.*G"; "%.*h"; "%.*H"; "%.*F";
      "%#.*F"; "%+.*f"; "% .*g"; "%*f"; "%-*g"; "%0*e"; "%+*F"; "%*h"; "%0*F";
      "%0*h"; "%-*.3f" ]
    (pairs fprecisions (some floats @ [ 0.; -0.; nan; infinity; 1e21; 5e-324 ]))
    (s2 si sf);
  test3
    [ "%*.*f"; "%-*.*e"; "%0*.*g"; "%+*.*F"; "%*.*h"; "%0*.*G" ]
    (triples widths [ -3; 0; 1; 5; 17; 30 ]
       [ 0.; -0.; 1.5; -1.5; 1e-7; 123456.789; 1e22; nan; neg_infinity ])
    (s3 si si sf);
  test2 [ "%*s"; "%-*s"; "%*S"; "%-*S" ] (pairs widths (some strings))
    (s2 si ss);
  test2 [ "%*B"; "%-*b" ] (pairs widths [ true; false ]) (s2 si string_of_bool);
  test2 [ "%c%C"; "%C%c"; "%c%c"; "%C%C" ]
    (pairs (some chars) [ '\''; '"'; '\\'; 'a'; '\n'; '\200' ])
    (s2 sc sc);
  test2 [ "%s at %d"; "%s%d"; "<%s|%5d>" ] (pairs (some strings) (some ints))
    (s2 ss si);
  test3 [ "%d of %d (%s)"; "%x-%o:%s" ]
    (triples (some ints) [ 0; -1; 42 ] [ ""; "x" ])
    (s3 si si ss);
  for _ = 1 to 2000 do
    let w = rand 61 - 30 and p = rand 40 and n = Int64.to_int (next ()) in
    let f = Int64.float_of_bits (next ()) and l = next () in
    let name = Printf.sprintf "fuzz %d %d %d %h %Ld" w p n f l in
    let both g h = check name g h in
    both (fun () -> Printf.sprintf "%*.*d|%-*x|%0*o" w p n w n w n)
      (fun () -> P.sprintf "%*.*d|%-*x|%0*o" w p n w n w n);
    both (fun () -> Printf.sprintf "%*.*f|%-*.*e|%0*.*g" w p f w p f w p f)
      (fun () -> P.sprintf "%*.*f|%-*.*e|%0*.*g" w p f w p f w p f);
    both (fun () -> Printf.sprintf "%*.*h|%*.*F|%.*G" w p f w p f p f)
      (fun () -> P.sprintf "%*.*h|%*.*F|%.*G" w p f w p f p f);
    both (fun () -> Printf.sprintf "%*.*Ld|%-*Lx|%#*Lo|%Lu" w p l w l w l l)
      (fun () -> P.sprintf "%*.*Ld|%-*Lx|%#*Lo|%Lu" w p l w l w l l);
    both (fun () -> Printf.sprintf "%#*d|%#*x|% *d|%+*d" w n w n w n w n)
      (fun () -> P.sprintf "%#*d|%#*x|% *d|%+*d" w n w n w n w n)
  done

let pa () x =
  note ("a " ^ x);
  "<" ^ x ^ ">"

let pt () =
  note "t";
  "T"

let ba b x =
  note ("a " ^ x);
  Buffer.add_string b ("<" ^ x ^ ">")

let bt b =
  note "t";
  Buffer.add_string b "T"

let custom () =
  Format
    ( String_literal
        ( "c:",
          Custom
            ( Custom_succ (Custom_succ Custom_zero),
              (fun () ->
                note "custom";
                fun s n ->
                  note "custom applied";
                  s ^ string_of_int n),
              Char_literal ('!', End_of_format) ) ),
      "c:%?!" )

let () =
  check "%a %t"
    (fun () -> Printf.sprintf "%a|%t|%a" pa "x" pt pa "y")
    (fun () -> P.sprintf "%a|%t|%a" pa "x" pt pa "y");
  check "%a invalid %a"
    (fun () -> Printf.sprintf "%a%_d%a" pa "1" pa "2")
    (fun () -> P.sprintf "%a%_d%a" pa "1" pa "2");
  check "%a scan char set"
    (fun () -> Printf.sprintf "%a%[a-z]%t" pa "1" "s" pt)
    (fun () -> P.sprintf "%a%[a-z]%t" pa "1" "s" pt);
  check "custom" (fun () -> Printf.sprintf (custom ()) "s" 4) (fun () -> P.sprintf (custom ()) "s" 4);
  check "custom partial"
    (fun () ->
      let f = Printf.sprintf (custom ()) in
      note "after format";
      let g = f "s" in
      note "after s";
      g 1 ^ g 2)
    (fun () ->
      let f = P.sprintf (custom ()) in
      note "after format";
      let g = f "s" in
      note "after s";
      g 1 ^ g 2);
  check "partial application"
    (fun () ->
      let f = Printf.sprintf "%d %a %d" 1 in
      f pa "x" 2 ^ f pa "y" 3)
    (fun () ->
      let f = P.sprintf "%d %a %d" 1 in
      f pa "x" 2 ^ f pa "y" 3);
  check "format arg"
    (fun () -> Printf.sprintf "<%{%d%s%a%t%}>" "%i%s%a%t")
    (fun () -> P.sprintf "<%{%d%s%a%t%}>" "%i%s%a%t");
  check "format arg nested"
    (fun () -> Printf.sprintf "<%{%(%d%)%{%c%}%ld%nd%Ld%f%B%}|%5{%S%}>" "%(%d%)%{%c%}%ld%nd%Ld%f%B" "%s")
    (fun () -> P.sprintf "<%{%(%d%)%{%c%}%ld%nd%Ld%f%B%}|%5{%S%}>" "%(%d%)%{%c%}%ld%nd%Ld%f%B" "%s");
  check "format subst"
    (fun () -> Printf.sprintf "<%(%d-%s%)>%d" "%d=%s" 1 "x" 2)
    (fun () -> P.sprintf "<%(%d-%s%)>%d" "%d=%s" 1 "x" 2);
  check "format subst nested"
    (fun () -> Printf.sprintf "<%(%(%d%)%)>" "[%(%d%)]" "%5d" 7)
    (fun () -> P.sprintf "<%(%(%d%)%)>" "[%(%d%)]" "%5d" 7);
  check "format subst with %a"
    (fun () -> Printf.sprintf "%(%a%t%)%a" "%a@[%t" pa "1" pt pa "2")
    (fun () -> P.sprintf "%(%a%t%)%a" "%a@[%t" pa "1" pt pa "2");
  check "format subst partial"
    (fun () ->
      let f = Printf.sprintf "%(%d%s%)!" "%d:%s" in
      f 1 "a" ^ f 2 "b")
    (fun () ->
      let f = P.sprintf "%(%d%s%)!" "%d:%s" in
      f 1 "a" ^ f 2 "b");
  check "ignored format subst"
    (fun () -> Printf.sprintf "<%_(%(%d%)%a%)%d>" "%d" 3 pa "x" 4)
    (fun () -> P.sprintf "<%_(%(%d%)%a%)%d>" "%d" 3 pa "x" 4);
  check "ignored format subst nested"
    (fun () -> Printf.sprintf "%_(%{%d%}%(%s%(%c%)%)%t%)%t" "%d" "%s%(%c%)" "s" "%c" 'c' pt pt)
    (fun () -> P.sprintf "%_(%{%d%}%(%s%(%c%)%)%t%)%t" "%d" "%s%(%c%)" "s" "%c" 'c' pt pt);
  check "open box"
    (fun () -> Printf.sprintf "a@[<%d>b%a@]c@{<%s>%t@}" 3 pa "x" "tag" pt)
    (fun () -> P.sprintf "a@[<%d>b%a@]c@{<%s>%t@}" 3 pa "x" "tag" pt);
  check "ksprintf"
    (fun () -> Printf.ksprintf (fun s -> note s; String.length s) "%s=%d%a" "x" 3 pa "y" |> string_of_int)
    (fun () -> P.ksprintf (fun s -> note s; String.length s) "%s=%d%a" "x" 3 pa "y" |> string_of_int);
  check "kprintf"
    (fun () -> (Printf.kprintf [@alert "-deprecated"]) (fun s -> note s; s ^ "!") "%s=%d%t" "x" 3 pt)
    (fun () -> (P.kprintf [@alert "-deprecated"]) (fun s -> note s; s ^ "!") "%s=%d%t" "x" 3 pt);
  check "ksprintf invalid"
    (fun () -> Printf.ksprintf (fun s -> note s; s) "%t%_d%t" pt pt)
    (fun () -> P.ksprintf (fun s -> note s; s) "%t%_d%t" pt pt);
  let buf name f g =
    check name
      (fun () ->
        let b = Buffer.create 4 in
        Buffer.add_string b "pre:";
        match f b with
        | () -> Buffer.contents b
        | exception e -> Buffer.contents b ^ " raised " ^ Printexc.to_string e)
      (fun () ->
        let b = Buffer.create 4 in
        Buffer.add_string b "pre:";
        match g b with
        | () -> Buffer.contents b
        | exception e -> Buffer.contents b ^ " raised " ^ Printexc.to_string e)
  in
  buf "bprintf"
    (fun b -> Printf.bprintf b "%s %d %5.2f %C %S %a %t %!@]%%" "x" 3 1.5 'c' "s" ba "y" bt)
    (fun b -> P.bprintf b "%s %d %5.2f %C %S %a %t %!@]%%" "x" 3 1.5 'c' "s" ba "y" bt);
  buf "bprintf invalid"
    (fun b -> Printf.bprintf b "%s%a%_d%t" "x" ba "y" bt)
    (fun b -> P.bprintf b "%s%a%_d%t" "x" ba "y" bt);
  buf "kbprintf"
    (fun b -> Printf.kbprintf (fun b -> note (Buffer.contents b); Buffer.add_char b '$') b "%d%a" 1 ba "z")
    (fun b -> P.kbprintf (fun b -> note (Buffer.contents b); Buffer.add_char b '$') b "%d%a" 1 ba "z");
  buf "ibprintf" (fun b -> Printf.ibprintf b "%d%a%t" 1 ba "z" bt)
    (fun b -> P.ibprintf b "%d%a%t" 1 ba "z" bt);
  let ik name f g = check name (fun () -> f (); "") (fun () -> g (); "") in
  let k () = note "k" in
  let stage f =
    note "applied";
    f
  in
  ik "ikfprintf"
    (fun () -> stage (stage (stage (Printf.ikfprintf k () "%d %s%a") 1) "s") pa "x")
    (fun () -> stage (stage (stage (P.ikfprintf k () "%d %s%a") 1) "s") pa "x");
  ik "ikfprintf all"
    (fun () ->
      stage (Printf.ikfprintf k () "%c%C%5s%*S%-5d%*.*x%ld%nd%Ld%f%*.*e%B%*b%a%t%!%{%d%}%l%[a-z]%0c@[<%d>%d@]%%")
        'a' 'b' "s" 1 "S" 1 1 2 3 1l (List.nth nativeints 1) 1L 1. 1 2 3. true 1 false pa "x" pt "%d" 4 "y" 'z' 5 6)
    (fun () ->
      stage (P.ikfprintf k () "%c%C%5s%*S%-5d%*.*x%ld%nd%Ld%f%*.*e%B%*b%a%t%!%{%d%}%l%[a-z]%0c@[<%d>%d@]%%")
        'a' 'b' "s" 1 "S" 1 1 2 3 1l (List.nth nativeints 1) 1L 1. 1 2 3. true 1 false pa "x" pt "%d" 4 "y" 'z' 5 6);
  ik "ikfprintf subst"
    (fun () -> stage (stage (Printf.ikfprintf k () "%(%d%s%)%d") "%d%s") 1 "x" 2)
    (fun () -> stage (stage (P.ikfprintf k () "%(%d%s%)%d") "%d%s") 1 "x" 2);
  ik "ikfprintf ignored"
    (fun () -> stage (stage (Printf.ikfprintf k () "%d%_d%a") 1) pa "x")
    (fun () -> stage (stage (P.ikfprintf k () "%d%_d%a") 1) pa "x");
  ik "ikfprintf ignored subst"
    (fun () -> stage (Printf.ikfprintf k () "%_(%d%s%)%d") 1 "s" 2)
    (fun () -> stage (P.ikfprintf k () "%_(%d%s%)%d") 1 "s" 2);
  ik "ikfprintf custom"
    (fun () ->
      let c = custom () in
      stage (stage (Printf.ikfprintf k () c) "s") 1 |> ignore)
    (fun () ->
      let c = custom () in
      stage (stage (P.ikfprintf k () c) "s") 1 |> ignore);
  ik "ifprintf" (fun () -> Printf.ifprintf () "%d%a" 1 pa "x")
    (fun () -> P.ifprintf () "%d%a" 1 pa "x");
  ik "ikbprintf"
    (fun () -> stage (Printf.ikbprintf (fun _ -> note "k") (Buffer.create 1) "%d%t") 1 bt)
    (fun () -> stage (P.ikbprintf (fun _ -> note "k") (Buffer.create 1) "%d%t") 1 bt)

let custom_arg () =
  Format
    ( Custom
        ( Custom_succ Custom_zero,
          (fun () ->
            note "custom arg";
            fun s -> "<" ^ s ^ ">"),
          End_of_format ),
      "%?" )

let boxed_custom_arg () =
  Format
    ( Formatting_gen
        ( Open_box (Format (String_literal ("<v>", End_of_format), "<v>")),
          Custom (Custom_succ Custom_zero, (fun () s -> s), End_of_format) ),
      "@[<v>%?" )

let caught f =
  match f () with s -> s | exception e -> "raised " ^ Printexc.to_string e

let exn_of f = match f () with _ -> None | exception e -> Some e

let describe reference e =
  match (reference, e) with
  | Some r, Some e ->
      Printf.sprintf "equal %b, slot id %d, slot name %s, %s" (r = e)
        (Printexc.exn_slot_id e) (Printexc.exn_slot_name e)
        (Printexc.to_string e)
  | _ -> "no exception"

let () =
  let both name f g = check name f g in
  both "subst %0c" (fun () -> Printf.sprintf "%(%c%)" "%0c" 'x')
    (fun () -> P.sprintf "%(%c%)" "%0c" 'x');
  both "subst [%0c]" (fun () -> Printf.sprintf "%(%c%)!" "[%0c]" 'y')
    (fun () -> P.sprintf "%(%c%)!" "[%0c]" 'y');
  both "Type_mismatch identity"
    (fun () ->
      let r = exn_of (fun () -> Printf.sprintf "%(%c%)" "%0c" 'x') in
      describe r (exn_of (fun () -> Printf.sprintf "%(%s%)" (custom_arg ()) "s")))
    (fun () ->
      let r = exn_of (fun () -> Printf.sprintf "%(%c%)" "%0c" 'x') in
      describe r (exn_of (fun () -> P.sprintf "%(%s%)" (custom_arg ()) "s")));
  both "subst %0c staged"
    (fun () ->
      let f = Printf.sprintf "%d%(%c%)" in
      note "format";
      let g = f 1 in
      note "int";
      let h = g "%0c" in
      note "arg";
      h 'z')
    (fun () ->
      let f = P.sprintf "%d%(%c%)" in
      note "format";
      let g = f 1 in
      note "int";
      let h = g "%0c" in
      note "arg";
      h 'z');
  both "subst after others"
    (fun () -> Printf.sprintf "%(%d%s%f%c%)" "%5d%-3s%.1f%0c" 1 "s" 2. 'c')
    (fun () -> P.sprintf "%(%d%s%f%c%)" "%5d%-3s%.1f%0c" 1 "s" 2. 'c');
  both "subst in box"
    (fun () -> Printf.sprintf "%(%s%c%)" "@[<v>%s@{<t>%0c@}@]" "s" 'c')
    (fun () -> P.sprintf "%(%s%c%)" "@[<v>%s@{<t>%0c@}@]" "s" 'c');
  both "subst after ignored"
    (fun () -> Printf.sprintf "%(%c%c%)" "%_d%c%!%0c" 'a' 'b')
    (fun () -> P.sprintf "%(%c%c%)" "%_d%c%!%0c" 'a' 'b');
  both "subst after ignored subst"
    (fun () -> Printf.sprintf "%(%_(%c%)%c%)" "%_(%c%)%0c" 'a' 'b')
    (fun () -> P.sprintf "%(%_(%c%)%c%)" "%_(%c%)%0c" 'a' 'b');
  both "subst nested inner"
    (fun () -> Printf.sprintf "%(%(%c%)%)" "<%(%c%)>" "%0c" 'n')
    (fun () -> P.sprintf "%(%(%c%)%)" "<%(%c%)>" "%0c" 'n');
  both "subst nested ok"
    (fun () -> Printf.sprintf "%(%(%c%)%)" "<%(%c%)>" "%c" 'n')
    (fun () -> P.sprintf "%(%(%c%)%)" "<%(%c%)>" "%c" 'n');
  both "subst format arg with %0c inside"
    (fun () -> Printf.sprintf "%(%{%c%}%)" "%{%0c%}" "%c")
    (fun () -> P.sprintf "%(%{%c%}%)" "%{%0c%}" "%c");
  both "subst scan conversions"
    (fun () -> Printf.sprintf "%(%s%d%c%)" "%[a-z]%l%c" "s" 3 'c')
    (fun () -> P.sprintf "%(%s%d%c%)" "%[a-z]%l%c" "s" 3 'c');
  both "subst arg for subst"
    (fun () -> Printf.sprintf "%(%(%d%)%)" "%{%d%}%d" "%d" 3)
    (fun () -> P.sprintf "%(%(%d%)%)" "%{%d%}%d" "%d" 3);
  both "subst padding star"
    (fun () -> Printf.sprintf "%(%*d|%d%d%)" "%d|%*d%d" 1 2 3 4)
    (fun () -> P.sprintf "%(%*d|%d%d%)" "%d|%*d%d" 1 2 3 4);
  both "subst precision star"
    (fun () -> Printf.sprintf "%(%.*f%d%)" "%d%f%d" 1 2. 3)
    (fun () -> P.sprintf "%(%.*f%d%)" "%d%f%d" 1 2. 3);
  both "subst variants"
    (fun () -> Printf.sprintf "%(%a%t%{%i%}%B%ld%Ld%nd%)" "%a%t%{%d%}%b%li%Lx%nu" pa "a" pt "%d" true 1l 2L (List.hd nativeints))
    (fun () -> P.sprintf "%(%a%t%{%i%}%B%ld%Ld%nd%)" "%a%t%{%d%}%b%li%Lx%nu" pa "a" pt "%d" true 1l 2L (List.hd nativeints));
  both "subst ignored kinds"
    (fun () -> Printf.sprintf "%(%d%)" "%_s%_c%_C%_S%_B%_f%_ld%_Ld%_nd%_{%d%}%_[a-z]%_0c%_l%d" 1)
    (fun () -> P.sprintf "%(%d%)" "%_s%_c%_C%_S%_B%_f%_ld%_Ld%_nd%_{%d%}%_[a-z]%_0c%_l%d" 1);
  both "subst ignored subst arg"
    (fun () -> Printf.sprintf "%(%_(%{%d%}%(%s%)%)%d%)" "%_(%{%d%}%(%s%)%)%d" "%d" "%s" "s" 1)
    (fun () -> P.sprintf "%(%_(%{%d%}%(%s%)%)%d%)" "%_(%{%d%}%(%s%)%)%d" "%d" "%s" "s" 1);
  both "subst custom"
    (fun () -> Printf.sprintf "%(%s%)" (custom_arg ()) "s")
    (fun () -> P.sprintf "%(%s%)" (custom_arg ()) "s");
  both "subst boxed custom"
    (fun () -> Printf.sprintf "%(%s%)" (boxed_custom_arg ()) "s")
    (fun () -> P.sprintf "%(%s%)" (boxed_custom_arg ()) "s");
  both "ifprintf subst %0c"
    (fun () -> Printf.ifprintf () "%(%c%)" "%0c" 'x'; "")
    (fun () -> P.ifprintf () "%(%c%)" "%0c" 'x'; "");
  both "ikfprintf subst %0c"
    (fun () -> Printf.ikfprintf (fun () -> note "k") () "%(%c%)" "%0c" 'x'; "")
    (fun () -> P.ikfprintf (fun () -> note "k") () "%(%c%)" "%0c" 'x'; "");
  both "ksprintf subst %0c"
    (fun () -> Printf.ksprintf (fun s -> s) "%(%c%)" "%0c" 'x')
    (fun () -> P.ksprintf (fun s -> s) "%(%c%)" "%0c" 'x');
  both "bprintf subst %0c"
    (fun () ->
      let b = Buffer.create 1 in
      caught (fun () -> Printf.bprintf b "a%(%c%)" "%0c" 'x'; Buffer.contents b)
      ^ Buffer.contents b)
    (fun () ->
      let b = Buffer.create 1 in
      caught (fun () -> P.bprintf b "a%(%c%)" "%0c" 'x'; Buffer.contents b)
      ^ Buffer.contents b);
  both "ibprintf subst custom"
    (fun () -> Printf.ibprintf (Buffer.create 1) "%(%s%)" (custom_arg ()) "s"; "")
    (fun () -> P.ibprintf (Buffer.create 1) "%(%s%)" (custom_arg ()) "s"; "")

let mark s = print_string ("\001" ^ s ^ "\001")

let chan name f g =
  let run_chan f =
    let r =
      match f () with
      | () -> "ok"
      | exception e -> "exception " ^ Printexc.to_string e
    in
    flush stdout;
    flush stderr;
    r
  in
  mark ("REF " ^ name);
  let e = run_chan f in
  mark ("TINY " ^ name);
  let a = run_chan g in
  mark "END";
  incr checks;
  if e <> a then fail name e a

let () =
  if js then begin
    let oa o x = output_string o ("<" ^ x ^ ">") in
    let ot o = output_string o "T\n" in
    chan "printf" (fun () -> Printf.printf "a%sb\n%d%c\n" "x" 3 'z')
      (fun () -> P.printf "a%sb\n%d%c\n" "x" 3 'z');
    chan "printf flush" (fun () -> Printf.printf "a%!b@.c%a\n%t" oa "x" ot)
      (fun () -> P.printf "a%!b@.c%a\n%t" oa "x" ot);
    chan "printf invalid" (fun () -> Printf.printf "a\nb%_dc\n")
      (fun () -> P.printf "a\nb%_dc\n");
    chan "eprintf" (fun () -> Printf.eprintf "e%s\nf%d\ng" "x" 3)
      (fun () -> P.eprintf "e%s\nf%d\ng" "x" 3);
    chan "eprintf lines" (fun () -> Printf.eprintf "%s%c%s\n%a%!x%t" "a\nb" '\n' "c" oa "q" ot)
      (fun () -> P.eprintf "%s%c%s\n%a%!x%t" "a\nb" '\n' "c" oa "q" ot);
    chan "eprintf invalid" (fun () -> Printf.eprintf "a\nb%[a-z]c\n" "s")
      (fun () -> P.eprintf "a\nb%[a-z]c\n" "s");
    chan "fprintf stdout" (fun () -> Printf.fprintf stdout "%5d|%-5s|\n" 3 "ab")
      (fun () -> P.fprintf stdout "%5d|%-5s|\n" 3 "ab");
    chan "fprintf stderr" (fun () -> Printf.fprintf stderr "@[%d@]@.%S\n" 3 "ab")
      (fun () -> P.fprintf stderr "@[%d@]@.%S\n" 3 "ab");
    chan "kfprintf"
      (fun () -> Printf.kfprintf (fun o -> output_string o "K\n") stderr "x%dy\n" 1)
      (fun () -> P.kfprintf (fun o -> output_string o "K\n") stderr "x%dy\n" 1);
    chan "ikfprintf"
      (fun () -> Printf.ikfprintf (fun o -> output_string o "K\n") stdout "x%dy\n" 1)
      (fun () -> P.ikfprintf (fun o -> output_string o "K\n") stdout "x%dy\n" 1);
    chan "printf subst %0c" (fun () -> Printf.printf "a\nb%(%c%)" "%0c" 'x')
      (fun () -> P.printf "a\nb%(%c%)" "%0c" 'x');
    chan "eprintf subst custom" (fun () -> Printf.eprintf "a\nb%(%s%)" (custom_arg ()) "s")
      (fun () -> P.eprintf "a\nb%(%s%)" (custom_arg ()) "s");
    chan "ifprintf" (fun () -> Printf.ifprintf stdout "x%dy\n" 1)
      (fun () -> P.ifprintf stdout "x%dy\n" 1)
  end;
  print_endline (si !checks ^ " checks, " ^ si !failures ^ " failures");
  if !failures > 0 then exit 1
