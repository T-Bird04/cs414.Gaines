(* Code generated and debugged using Grok*)
module DList : sig
  type 'a node = {
    value : 'a;
    mutable prev : 'a node option;
    mutable next : 'a node option;
  }

  type 'a t = {
    mutable head : 'a node option;
    mutable tail : 'a node option;
    mutable size : int;
  }

  val create : unit -> 'a t
  val is_empty : 'a t -> bool
  val size : 'a t -> int
  val push_front : 'a t -> 'a -> unit
  val push_back : 'a t -> 'a -> unit
  val pop_front : 'a t -> 'a option
  val pop_back : 'a t -> 'a option
  val remove : 'a t -> 'a -> unit  (* Removes first occurrence *)
  val iter_forward : 'a t -> ('a -> unit) -> unit
  val iter_backward : 'a t -> ('a -> unit) -> unit
  val to_list : 'a t -> 'a list  (* For easy inspection *)
end = struct
  type 'a node = {
    value : 'a;
    mutable prev : 'a node option;
    mutable next : 'a node option;
  }

  type 'a t = {
    mutable head : 'a node option;
    mutable tail : 'a node option;
    mutable size : int;
  }

  let create () = { head = None; tail = None; size = 0 }

  let is_empty dl = dl.size = 0

  let size dl = dl.size

  let make_node v = { value = v; prev = None; next = None }

  let push_front dl v =
    let node = make_node v in
    match dl.head with
    | None ->
        dl.head <- Some node;
        dl.tail <- Some node;
        dl.size <- 1
    | Some old_head ->
        node.next <- Some old_head;
        old_head.prev <- Some node;
        dl.head <- Some node;
        dl.size <- dl.size + 1

  let push_back dl v =
    let node = make_node v in
    match dl.tail with
    | None ->
        dl.head <- Some node;
        dl.tail <- Some node;
        dl.size <- 1
    | Some old_tail ->
        node.prev <- Some old_tail;
        old_tail.next <- Some node;
        dl.tail <- Some node;
        dl.size <- dl.size + 1

  let pop_front dl =
    match dl.head with
    | None -> None
    | Some node ->
        dl.head <- node.next;
        (match dl.head with
         | Some new_head -> new_head.prev <- None
         | None -> dl.tail <- None);
        dl.size <- dl.size - 1;
        Some node.value

  let pop_back dl =
    match dl.tail with
    | None -> None
    | Some node ->
        dl.tail <- node.prev;
        (match dl.tail with
         | Some new_tail -> new_tail.next <- None
         | None -> dl.head <- None);
        dl.size <- dl.size - 1;
        Some node.value

  let remove dl v =
    let rec find_and_remove current =
      match current with
      | None -> ()
      | Some node ->
          if node.value = v then (
            (match node.prev with
             | Some prev -> prev.next <- node.next
             | None -> dl.head <- node.next);
            (match node.next with
             | Some next -> next.prev <- node.prev
             | None -> dl.tail <- node.prev);
            dl.size <- dl.size - 1
          ) else find_and_remove node.next
    in
    find_and_remove dl.head

  let iter_forward dl f =
    let rec iter node =
      match node with
      | None -> ()
      | Some n ->
          f n.value;
          iter n.next
    in
    iter dl.head

  let iter_backward dl f =
    let rec iter node =
      match node with
      | None -> ()
      | Some n ->
          f n.value;
          iter n.prev
    in
    iter dl.tail

  let to_list dl =
    let rec build acc node =
      match node with
      | None -> List.rev acc
      | Some n -> build (n.value :: acc) n.next
    in
    build [] dl.head
end


let dl = DList.create ();;
DList.push_back dl 1;;
DList.push_back dl 2;;
DList.push_front dl 0;;
DList.iter_forward dl print_int;;
print_newline ();;
DList.iter_backward dl print_int;; 
print_newline ();;
ignore (DList.pop_back dl);;
ignore (DList.is_empty);;
ignore (DList.pop_front);;
ignore (DList.to_list);;
Printf.printf "Size: %d\n" (DList.size dl);;
DList.remove dl 1;;
Printf.printf "Size: %d\n" (DList.size dl);;