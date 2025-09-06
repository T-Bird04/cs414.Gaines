(*Code Sourced from  https://github.com/adamwadelewis/cs414.lewis.fa25/blob/main/samples/module01/treedemo/src/treedemo.ml*)
open Printf


let values = [10; 5; 15]

let aBintree = List.fold_left (fun t x -> Bintree.Tree.insert t x) Bintree.Tree.Empty values
    
let tree_as_string = Bintree.Tree.string_of_tree aBintree
let () = print_endline tree_as_string

(* Prune Test *) 
let prune_string = Bintree.Tree.string_of_tree (Bintree.Tree.prune_tree aBintree)
let () = print_endline prune_string

(* Level Traversal Test*)
let levelTraversedTree = Bintree.Tree.level_traversal aBintree
let () = List.iter (printf "%d ") levelTraversedTree
(* List.iter sourced from https://stackoverflow.com/questions/9134929/print-a-list-in-ocaml*)