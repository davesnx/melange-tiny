module H = Stdlib.Hashtbl

external log : 'a -> unit = "log" [@@mel.scope "console"]

let t : (string, int) H.t = H.create 16
let u : (int, string) H.t = H.create 16

let () =
  H.replace t "a" 1;
  H.replace t "b" 2;
  H.replace u 1 "one";
  if H.mem t "a" then H.remove t "a";
  H.iter (fun k v -> log (k, v)) t;
  log (H.fold (fun _ v acc -> acc + v) t 0);
  log (H.find_opt t "b");
  log (H.find u 1);
  log (H.length t);
  H.reset t;
  H.clear u
