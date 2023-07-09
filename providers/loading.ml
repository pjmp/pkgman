open Lwt

let spinner ~message =
  let symbols = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |] in
  let rec loop i =
    let symbol = symbols.(i mod Array.length symbols) in
    Lwt_io.print ("\r" ^ symbol ^ " " ^ message) >>= fun () ->
    Lwt_unix.sleep 0.15 >>= fun () -> loop (i + 1)
  in
  loop 0

(* open Progress

   let bar ~message ~total =
     let open Line.Using_int64 in
     list
       [
         rpad 11 (constf " %s" message);
         bytes;
         bytes_per_sec;
         bar total;
         percentage_of total ++ const " ";
       ]

   let spinner ~message ~total ~current =
     let layout = bar ~message ~total in
     with_reporter layout @@ fun f ->
     f current;
     Unix.sleepf 0.001 *)
