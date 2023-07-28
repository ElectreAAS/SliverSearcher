module SearchTest (S : Searcher.S) = struct
  let data =
    [|
      ("", "text", Some 0);
      ("aa", "acababaa", Some 6);
      ("hello", "hellholleheolhelloheheyhello", Some 13);
      ("test", "nothing to see here", None);
    |]

  let test () =
    Array.iter
      (fun (pat, text, expected) ->
        Alcotest.(check (option int))
          "Search should be correct" expected (S.search ~pat text))
      data

  let tests = (S.name, [ ("Tests", `Quick, test) ])
end

let v (module S : Searcher.S) =
  let module T = SearchTest (S) in
  T.tests

let test_suite =
  [
    v (module Searcher.KMP); v (module Searcher.Opti); v (module Searcher.Naive);
  ]

let () = Alcotest.run "All" test_suite
