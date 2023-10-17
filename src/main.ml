module GP = Gnuplot

let algos : (module Searcher.S) array =
  [|
    (module Searcher.Naive);
    (module Searcher.Opti);
    (module Searcher.KMP);
    (module Searcher.BMH);
    (module Searcher.BM);
  |]

(** generates 'a', 'b', 'c', or 'd'. *)
let random_char _ = Random.int 4 + 97 |> Char.chr

let random_string len = String.init len random_char

let string_gen needle h =
  let before = random_string (3 * h / 4) and after = random_string (h / 4) in
  before ^ needle ^ after

let generate h =
  let n = 1 + Random.int (h - 1) in
  let needle = random_string n in
  let haystack = string_gen needle h in
  (needle, haystack)

let _gen_determinist p text_size =
  let pat = String.init p (fun x -> if x = p - 1 then 'b' else 'a') in
  let text = String.make text_size 'a' in
  (pat, text)

let mmm data f =
  let results = Array.map f data in
  Array.fast_sort Int.compare results;
  let min, max =
    Array.fold_left
      (fun (mi, ma) res -> (min mi res, max ma res))
      (max_int, min_int) results
  in
  (min, results.(Array.length results / 2), max)

let bench_all ~iter ~max_h =
  let init = Array.map (fun (module M : Searcher.S) -> (M.name, [||])) algos in
  let rec loop acc i =
    if i > max_h then acc
    else (
      Format.printf "Entering loop of size %d@." i;
      let pairs = Array.init iter (fun _ -> generate i) in
      let _resssss =
        Array.fold_left
          (fun acc_modules (module M : Searcher.S) ->
            Array.fold_left
              (fun acc_data (needle, haystack) ->
                float (M.search_count ~needle ~haystack) :: acc_data)
              [] pairs
            :: acc_modules)
          [] algos
      in

      let results =
        Array.map
          (fun (module M : Searcher.S) ->
            ( M.name,
              M.index,
              mmm pairs (fun (needle, haystack) ->
                  M.search_count ~needle ~haystack) ))
          algos
      in
      let results =
        Array.map
          (fun (name, _index, (m1, m2, m3)) ->
            (name, [| (i, m1); (i, m2); (i, m3) |]))
          results
      in
      let acc' =
        Array.map2
          (fun (name, res_acc) (nname, new_res) ->
            assert (name = nname);
            (name, Array.append new_res res_acc))
          acc results
      in
      loop acc' (i + 4096))
  in
  loop init 0

let _log_bench oc name res =
  Printf.fprintf oc "# %s\n# Size | Comparisons\n" name;
  Array.iter (fun (len, time) -> Printf.fprintf oc "%d %d\n" len time) res;
  output_string oc "\n\n"

let log_all oc all =
  Printf.fprintf oc "Size";
  Array.iter (fun (name, _) -> Printf.fprintf oc " %s" name) all;
  for i = 0 to Array.length (snd all.(0)) - 1 do
    Printf.fprintf oc "\n%d" (fst (snd all.(0)).(i));
    Array.iter
      (fun (_, res) ->
        let _, n = res.(i) in
        Printf.fprintf oc " %d" n)
      all
  done

let plot data =
  let gp = GP.create ~verbose:true ~path:"/usr/bin/gnuplot" () in
  let output = GP.Output.create ~size:(1920, 1080) (`Png_cairo "ograph.png") in
  let title = "String-search algorithm comparisons" in
  let labels = GP.Labels.create ~x:"Size" ~y:"Number of comparisons" () in
  GP.set ~output ~title ~labels gp;
  GP.plot_many gp data;
  GP.close gp

let () =
  Random.self_init ();
  let max_h = 4096 * 16 in
  let iter = 256 in
  let oc = open_out "search.dat" in
  let results = bench_all ~iter ~max_h in
  log_all oc results;
  (* Array.iter (fun (name, res) -> log_bench oc name res) results; *)
  close_out oc
