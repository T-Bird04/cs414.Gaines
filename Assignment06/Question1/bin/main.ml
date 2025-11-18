(*Code Generate by Grok using multiple prompts*)

[@@@ocaml.warning "-32"] (*Ignores unused values*)

module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
  (* Optional: map and apply derived from return/bind *)
  val map    : ('a -> 'b) -> 'a t -> 'b t
end

module Make_infix (M : MONAD) = struct
  let ( >>= ) = M.bind
  let ( let* ) = M.bind      (* for let-operators *)
  let ( >|= ) m f = M.map f m
end

module OptionM : MONAD = struct
  type 'a t = 'a option
  let return x = Some x
  let bind m f = match m with None -> None | Some x -> f x
  let map f m = match m with None -> None | Some x -> Some (f x)
end
module O = Make_infix(OptionM)

module ResultM = struct
  type 'a t = ('a, string) result
  let return x = Ok x
  let bind m f = match m with Error e -> Error e | Ok x -> f x
  let map f m = match m with Error e -> Error e | Ok x -> Ok (f x)
end
module R = Make_infix((ResultM : MONAD with type 'a t = ('a, string) result))  (* Preserve type equality *)

module ListM : MONAD = struct
  type 'a t = 'a list
  let return x = [x]
  let bind xs f = List.concat (List.map f xs)
  let map f xs = List.map f xs
end
module L = Make_infix(ListM)

let safe_div a b =
  if b = 0. then Error "Division by zero"
  else Ok (a /. b)

let testEquation x y z =
  let open R in
  let* q = safe_div x y in
  safe_div q z

(* Example calls - wrap in let () = for execution in .ml file *)
let () =
  (match testEquation 10. 20. 30. with
   | Ok v -> Printf.printf "Ok %f\n" v
   | Error e -> Printf.printf "Error \"%s\"\n" e);
 