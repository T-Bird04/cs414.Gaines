(* Generated with Grok using part 2 a initial prompt and Debugged with ChatGPT *)

[@@@ocaml.warning "-32"]

open Printf

(*--------------------------------------*)
(* Merge function for mergesort         *)
(*--------------------------------------*)
let rec merge a b =
  match a, b with
  | [], l | l, [] -> l
  | h1 :: t1, h2 :: t2 ->
      if h1 <= h2 then h1 :: merge t1 (h2 :: t2)
      else h2 :: merge (h1 :: t1) t2

(*--------------------------------------*)
(* Mergesort implementation             *)
(*--------------------------------------*)
let rec mergesort l =
  let len = List.length l in
  if len <= 1 then l
  else
    let mid = len / 2 in
    (* split list into (left, right) *)
    let rec take n lst acc =
      if n = 0 then List.rev acc, lst
      else match lst with
        | [] -> List.rev acc, []
        | h :: t -> take (n - 1) t (h :: acc)
    in
    let left, right = take mid l [] in
    merge (mergesort left) (mergesort right)

(*--------------------------------------*)
(* Bubblesort (mutating array version)  *)
(*--------------------------------------*)
let bubblesort lst =
  let arr = Array.of_list lst in
  let n = Array.length arr in
  for i = 0 to n - 2 do
    for j = 0 to n - i - 2 do
      if arr.(j) > arr.(j + 1) then begin
        let temp = arr.(j) in
        arr.(j) <- arr.(j + 1);
        arr.(j + 1) <- temp
      end
    done
  done;
  Array.to_list arr

(*--------------------------------------*)
(* Quicksort placeholder                *)
(*--------------------------------------*)
let quicksort lst = List.sort compare lst

(*--------------------------------------*)
(* Insertionsort                        *)
(*--------------------------------------*)
let insertionsort lst =
  let rec insert sorted x =
    match sorted with
    | [] -> [x]
    | h :: t as l ->
        if x <= h then x :: l
        else h :: insert t x
  in
  List.fold_left insert [] lst

(*--------------------------------------*)
(* Higher-order function for sorting    *)
(*--------------------------------------*)
let sort_with strategy lst = strategy lst

(*--------------------------------------*)
(* SortContext module                   *)
(*--------------------------------------*)
module SortContext = struct
  let strategy = ref (fun (lst : int list) -> lst)

  let set_strategy s = strategy := s

  let execute_strategy lst =
    let start = Sys.time () in
    let result = !strategy lst in
    let elapsed = (Sys.time () -. start) *. 1000.0 in
    printf "Execution time: %.3f ms\n%!" elapsed;
    result
end

(*--------------------------------------*)
(* Functor-based parameterization        *)
(*--------------------------------------*)
module type SORT = sig
  val sort : int list -> int list
end

module MakeSortContext (S : SORT) = struct
  let execute_strategy lst = S.sort lst
end

(*--------------------------------------*)
(* Strategy modules for functor         *)
(*--------------------------------------*)
module Quick = struct let sort = quicksort end
module Merge = struct let sort = mergesort end
module Bubble = struct let sort = bubblesort end
module Insertion = struct let sort = insertionsort end

(*--------------------------------------*)
(* Functor context declaration          *)
(* MUST be outside let () block         *)
(*--------------------------------------*)
module QuickContext = MakeSortContext(Quick)

(*--------------------------------------*)
(* Printing helper                      *)
(*--------------------------------------*)
let print_list lst =
  List.iter (fun x -> printf "%d " x) lst;
  print_newline ()

(*--------------------------------------*)
(* Main program                         *)
(*--------------------------------------*)
let () =
  let original = [5; 2; 9; 1; 5; 6] in

  printf "Simple sort_with quicksort:\n";
  let sorted_simple = sort_with quicksort original in
  print_list sorted_simple;
  print_newline ();

  if Array.length Sys.argv > 1 then begin
    (* Command-line selection *)
    let arg = Sys.argv.(1) in
    let valid = ref true in

    begin match arg with
    | "quick"     -> SortContext.set_strategy quicksort
    | "merge"     -> SortContext.set_strategy mergesort
    | "bubble"    -> SortContext.set_strategy bubblesort
    | "insertion" -> SortContext.set_strategy insertionsort
    | _ ->
        valid := false;
        printf "Unknown strategy: %s\n" arg
    end;

    if !valid then begin
      printf "Selected strategy (%s):\n" arg;
      let sorted = SortContext.execute_strategy original in
      print_list sorted
    end
  end

  else begin
    (* Run all with timing *)
    printf "Quicksort:\n";
    SortContext.set_strategy quicksort;
    print_list (SortContext.execute_strategy original);
    print_newline ();

    printf "Mergesort:\n";
    SortContext.set_strategy mergesort;
    print_list (SortContext.execute_strategy original);
    print_newline ();

    printf "Bubblesort:\n";
    SortContext.set_strategy bubblesort;
    print_list (SortContext.execute_strategy original);
    print_newline ();

    printf "Insertionsort:\n";
    SortContext.set_strategy insertionsort;
    print_list (SortContext.execute_strategy original);
    print_newline ();
  end;

  (* Functor example *)
  printf "Functor example with Quick:\n";
  let sorted_functor = QuickContext.execute_strategy original in
  print_list sorted_functor
