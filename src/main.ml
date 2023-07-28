Random.self_init ()

let random_char _ = Random.int 256 |> Char.chr
let random_string len = String.init len random_char

let string_gen ?pat n =
  match pat with
  | None -> random_string n
  | Some pat ->
      let p = String.length pat in
      let half = max 0 ((n / 2) - p) in
      let before = random_string half and after = random_string half in
      before ^ pat ^ after

let _generate n =
  let p = Random.int n in
  let pat = random_string p in
  let text = if Random.bool () then string_gen ~pat n else string_gen n in
  (pat, text)

let gen_determinist p text_size =
  let pat = String.init p (fun x -> if x = p - 1 then 'b' else 'a') in
  let text = String.make text_size 'a' in
  (pat, text)

let bench_worst_case (module M : Searcher.S) ~iter ~max ~text_size =
  let rec loop acc p =
    if p < 2 then acc
    else (
      Format.printf "Entering loop of size %d (max is %d) for %s@." p max M.name;
      let timers =
        Array.init iter (fun _ ->
            let pat, text = gen_determinist p text_size in
            let counter = Mtime_clock.counter () in
            ignore @@ Sys.opaque_identity (M.search ~pat text);
            let span =
              (Mtime_clock.count counter |> Mtime.Span.to_float_ns) /. 1e3
            in
            span)
      in
      Array.fast_sort Float.compare timers;
      let spans = Array.to_list @@ Array.map (fun span -> (span, p)) timers in
      loop (spans @ acc) (p / 2))
  in
  let res = loop [] max in
  (M.name, res)

let log_bench oc (name, res) =
  Printf.fprintf oc "# %s\n#Size | Time (µs)\n" name;
  List.iter (fun (time, len) -> Printf.fprintf oc "%d %f\n" len time) res;
  output_string oc "\n\n"

let () =
  let iter = 100 in
  let text_size = 16_384 in
  let max = 4096 in
  let oc = open_out "search.dat" in
  let bench (module M : Searcher.S) =
    let res = bench_worst_case (module M) ~iter ~max ~text_size in
    log_bench oc res
  in
  bench (module Searcher.Naive);
  bench (module Searcher.Opti);
  bench (module Searcher.KMP);
  bench (module Searcher.BMH);
  close_out oc
