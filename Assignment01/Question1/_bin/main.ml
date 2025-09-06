(* Sources https://ocaml.org/docs/loops-recursion
https://athensstate.instructure.com/courses/9004/assignments/238091 
Google AI overview (Google Search: ocaml how to declare variable type in function) *)


(* nat data type *)
type nat = 
  | Z (* Represents zero *)
  | S of nat (* Represents the successor of a natural number *);;

(* nat to int conversion *)
let rec to_int : nat -> int = function
  | Z -> 0
  | S n -> 1 + to_int n;;


(*
Grok Prompt:
is this bad ocaml code: 
(* nat data type *)
type nat = 
  | Z (* Represents zero *)
  | S of nat (* Represents the successor of a natural number *);;

(* nat to int conversion *)
let rec to_int : nat -> int = function
  | Z -> 0
  | S n -> 1 + to_int n;;

(* Addition *)
let add_nat (a : nat) (b : nat) = function
  |a -> Z
  c = to_int b
  |b -> Z
  c = to_int a
  |c -> to_int a to_int b;;

(* Mult *)
let mult_nat (a : nat) (b : nat) = function
  | _ -> Z
  c = 0
  |for 0 to a
   do add_nat c b
   done;;

(* main *)
let () = 
printf add_nat S(S(S(Z))) S(S(Z))
printf mult_nat Z S(Z)
*)

(* Code provided by Grok *)

(* Addition *)
let rec add_nat (a : nat) (b : nat) : nat =
  match a with
  | Z -> b
  | S a' -> S (add_nat a' b)

(* Code provided by Grok *)

(* Multiplication *)
let rec mult_nat (a : nat) (b : nat) : nat =
  match a with
  | Z -> Z
  | S a' -> add_nat b (mult_nat a' b)


(* Code provided by Grok *)

(* Main function *)
let () =
  let result1 = add_nat (S (S (S Z))) (S (S Z)) in
  let result2 = mult_nat Z (S Z) in
  Printf.printf "add_nat (S (S (S Z))) (S (S Z)) = %d\n" (to_int result1);
  Printf.printf "mult_nat Z (S Z) = %d\n" (to_int result2)
