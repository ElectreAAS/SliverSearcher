let algos : (module Searcher.S) array =
  [|
    (module Searcher.Naive);
    (module Searcher.Opti);
    (module Searcher.KMP);
    (module Searcher.BMH);
    (module Searcher.BM);
  |]

module ManualTests (S : Searcher.S) = struct
  let data =
    [|
      ("example", "Here is a simple example", Some 17);
      ("adb", "abdbabdbababadbdbdabdabad", Some 12);
      ("", "", Some 0);
      ("a", "", None);
      ("", "text", Some 0);
      ("aaaaba", "aaaacaaaaaa", None);
      ("cbcc", "aaaaabccbcc", Some 7);
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

let manuals =
  let manual (module S : Searcher.S) =
    let module T = ManualTests (S) in
    T.tests
  in
  ("Manual tests", Array.to_list @@ Array.map manual algos)

let qcheckers =
  let char_gen = QCheck2.Gen.oneofl [ 'a'; 'b'; 'c'; 'd' ] in
  let str_gen = QCheck2.Gen.(string_size ~gen:char_gen small_nat) in
  let pair_gen =
    QCheck2.Gen.(
      let+ needle = str_gen and+ prefix = str_gen and+ suffix = str_gen in
      let haystack = prefix ^ needle ^ suffix in
      (needle, haystack))
  in
  let qcheck (module S : Searcher.S) =
    QCheck2.(
      Test.make ~count:1_000 ~name:S.name
        ~print:Print.(pair string string)
        pair_gen
        (fun (needle, haystack) ->
          let re = Str.regexp_string needle in
          let stdlib =
            match Str.search_forward re haystack 0 with
            | n -> Some n
            | exception Not_found -> None
          in
          S.search ~needle ~haystack = stdlib))
    |> QCheck_alcotest.to_alcotest
  in
  ("Quickcheck", Array.to_list @@ Array.map qcheck algos)

module ExactTests = struct
  let data =
    [
      ("hello", "xxxxhello", 6, 3, "BMH on trivial example");
      ("hello", "xxxxxhello", 6, 3, "BMH on trivial example 2");
      ("hello", "xxxxhello", 6, 4, "BM on trivial example");
      ("hello", "xxxxxhello", 6, 4, "BM on trivial example 2");
      ("aa", "abababababaa", 7, 3, "BMH on simple pattern");
      ("aa", "abababababaa", 7, 4, "BM on simple pattern");
      ("anampnam", "manpanampnama", 12, 4, "BM on wikipedia example");
      ("example", "Here is a simple example.", 15, 4, "BM on Moore's example");
      ("adb", "abdbabdbababadbdbdabdabad", 13, 4, "Slightly more repetitive");
    ]
end

let exacts =
  List.map
    (fun (needle, haystack, count, index, name) ->
      let (module M : Searcher.S) = algos.(index) in
      Alcotest.test_case name `Quick @@ fun () ->
      Alcotest.(check int)
        "Count should be correct" count
        (M.search_count ~needle ~haystack))
    ExactTests.data

let exacts = ("Exact comparisons", exacts)
let () = Alcotest.run "All" [ qcheckers; manuals; exacts ]
