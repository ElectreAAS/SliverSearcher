module ManualTests (S : Searcher.S) = struct
  let data =
    [|
      ("", "", Some 0);
      ("a", "", None);
      ("", "text", Some 0);
      ("aaaaba", "aaaacaaaaaa", None);
      ("cbcc", "aaaaabccbcc", Some 7);
      ("ab", "aaba", Some 1);
      ("acc", "aaaabaaaaaacc", Some 10);
      ("aa", "acababaa", Some 6);
      ("hello", "hellholleheolhelloheheyhello", Some 13);
      ("test", "nothing to see here", None);
      ( "cadbbcbc",
        "aaaaaaaaaaaaaaaaaaaaaaaaabaaacadbbcbcaaaaaaaaaaaaaaa",
        Some 29 );
    |]

  let test () =
    Array.iter
      (fun (needle, haystack, expected) ->
        Alcotest.(check (option int))
          "Search should be correct" expected
          (S.search ~needle ~haystack))
      data

  let tests = (S.name, `Quick, test)
end

let on_all_modules (f : (module Searcher.S) -> unit Alcotest.test_case) =
  [
    f (module Searcher.Naive);
    f (module Searcher.KMP);
    f (module Searcher.Opti);
    f (module Searcher.BMH);
    f (module Searcher.BM);
  ]

let manuals =
  let manual (module S : Searcher.S) =
    let module T = ManualTests (S) in
    T.tests
  in
  ("Manual tests", on_all_modules manual)

let qcheckers =
  let char_gen = QCheck2.Gen.oneofl [ 'a'; 'b'; 'c' ] in
  let str_gen = QCheck2.Gen.(string_size ~gen:char_gen small_nat) in
  let qcheck (module S : Searcher.S) =
    let total = ref 0 in
    let found = ref 0 in
    at_exit (fun () ->
        Printf.printf "\n%s: %d / %d had the needle inside\n%!" S.name !found
          !total);
    QCheck2.(
      Test.make ~count:1_000 ~name:S.name
        ~print:Print.(pair string string)
        Gen.(pair str_gen str_gen)
        (fun (needle, haystack) ->
          assume (String.length needle <= String.length haystack);
          incr total;
          let re = Str.regexp_string needle in
          let stdlib =
            match Str.search_forward re haystack 0 with
            | n ->
                incr found;
                Some n
            | exception Not_found -> None
          in
          S.search ~needle ~haystack = stdlib))
    |> QCheck_alcotest.to_alcotest
  in
  ("Quickcheck", on_all_modules qcheck)

let () = Alcotest.run "All" [ qcheckers; manuals ]
