let () = print_endline "Hello, World!"
let values = [10; 5; 15]

let aBintree = List.fold_left (fun t x -> Bintree.Tree.insert t x) Bintree.Tree.Empty values