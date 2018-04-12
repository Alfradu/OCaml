(*Simple phone book*)
module type PHONEBOOK =
  sig
    type fbook = (string * string) list
    exception PersonNotFound
    exception PersonTooFat
    val emptyfbook : fbook
    val add_entry : string * string * fbook -> fbook
    val delete_entry : string * fbook -> fbook
    val display_book : fbook -> string
  end;;

module ListPhoneBook : PHONEBOOK =
  struct
    exception PersonNotFound
    exception PersonTooFat
    type fbook = (string * string) list
    let emptyfbook = []
    let rec add_entry (person, number, book) = match book with
      [] -> [fbook(person, number)] (* <- Konstigt syntax-fel *)
      | [(p,n)::t] -> if p = person
                      then (p, number)::t
                      else (p,n)::(add_entry(person, number, book))
    let rec delete_entry (person, book) =
      match book with
        [] -> raise PersonNotFound
      | [(p,n)::t] -> if (person = p)
                      then t
                      else (p,n)::(delete_entry(person, t))
    let rec query_entry (person, book) =
      match book with
        [] -> raise PersonNotFound
      | [(p,n)::t] -> if person = p
                      then n
                      else query_entry(person, t)
    let rec display_book book =
      match book with
        [] -> ""
      | [(p,n)] -> "(" ^ p ^ ", " ^ n ^ ")"
      | [(p,n)::t] -> "(" ^ p ^ ", " ^ n ^ ")" ^ (display_book t)
end;;

  struct
  module ArrayPhoneBook : PHONEBOOK =
    type fbook = (string * string) array
    exception PersonNotFound
    exception PersonTooFat
    let emptyfbook = Array.make 10 ("","")
    let c = ref 0
    let fst (a,b) = a
    let snd (a,b) = b
    let search (name,book) =
      let rec find (i,name,book) =
        if i = !c
        then -1
        else
          if (fst (book.(i))) = name
          then i
          else find (i+1,name,book)
      in
        find (0,name,book)

    let add_entry (person, number, book) =
      let pos = search (person, book)
      in if pos = -1
        then if c > 10
          then raise PersonTooFat
          else (
            book.(!c) <- (person, number);
            c := !c + 1;
            book)
        else snd(book.(pos)) <- number;
             book

    let delete_entry (person, book) =
      let pos = search (person, book)
      in if pos = -1
         then raise PersonNotFound
         else book.(pos) <- book.(!c-1);
              c := !c - 1;
              book

    let query_entry (person, book) =
      let pos = search (person, book)
      in if pos = -1
         then raise PersonNotFound
         else snd (book.(pos))

    let display_book book =
      let size = !c
      in
        if size > 0
        then
          let rec display_b (book, index) =
            let (person, phone) = book.(index) in
            let pretty = "(" ^ person ^ "," ^ phone ^ ")"
            in if index = size-1
               then pretty
               else pretty ^ (display_b (book, index+1))
          in
            display_b (book, 0)
        else ""
      end;;

(* Lineär Search *)
exception Search_Failed;;
let rec search (key, a, f, l) =
  if f>l
  then raise Search_Failed
  else (let (k, v) = a.(f)
        in if key = k
          then v
          else search (key, a ,f+1, l));;

  let arr = [|(3,"Steve");(2,"Bob")|];;

# search (2,arr,0,1);;
# - : string = "Bob"

(* Bineär Särch*)
let rec bsearch (key, a, l, r) =
  if l>r
  then raise Search_Failed
  else (let mid = (l+r)/2 in
        let (k,v) = a.(mid)
        in if key=k
          then v
          else (if k>key
                then bsearch (key,a,l,mid-1)
                else bsearch (key,a,mid+1,r) ));;

# let arr2 = [|(2,'a');(3,'b');(5,'c');(6,'d');(7,'e');(8,'f');(9,'h')|];;
# bsearch (5,arr,0,6);;
- : char = 'c'# bsearch (9,arr,0,6);;
- : char = 'h'

(*Hash functions, modulo and stuff, buckets*)

(*Quicksort*)
(*
"Divide and conquer" algoritm: delar upp stora arrayer i två små
sub-arrayer som blir de lägre elementen och de höga elementen.

Består av tre steg: Pivotering, Partitionering, Rekursion.
Pivotering:
väljer ett element från arrayen.
Partitionering:
Arrangerar om arrayen så att alla element som är
mindre än det valda "pivot"-elementet hamnar innan "pivot"-elementet
högre värden hamnar efter.
Rekursion:
detta appliceras om och om igen på alla sub-arrayer *)

(* Partiotioning for Quicksort*)
let rec split (a, pivot, i, j, hi)
  if i>j
  then (swap (a,i,hi); i)
  else if a.(i) <= pivot
       then split (a,pivot,i+1,j,hi)
       else if (a.(i) > pivot) && (a.(j) <= pivot)
            then (swap (a,i,j);
              split (a,pivot,i+1,j-1,hi) )
            else split (a,pivot,i,j-1,hi) ;;

let partition (a,lo,hi) = split (a,a.(hi),lo,hi-1,hi);;
(*Denna partitionering låter oss placera lägre element under pivoten
och högre element över pivoten. Den ger oss inte en sorterad lista.*)

(* Trees *)
(* In-order traversal *)
let rec inorder t =
  match t with
    Leaf             -> []
  | (Node (t1,n,t2)) -> (inorder t1) @ (n::(inorder t2))

(* Pre-order traversal *)
let rec preorder t =
  match t with
    Leaf             -> []
  | (Node (t1,n,t2)) -> n::(preorder(t1) @ preorder(t2))

(* Post-order traversal *)
(* depth-first, left->right*)
let rec postorder t =
  match t with
    Leaf             -> []
  | (Node (t1,n,t2)) -> (postorder(t1) @ postorder(t2)) @ [n]

(* Breadth first traversal with queues *)
let breadthfirst t = bfst (enq(empty, t))

let bfst q =
  if qnull q
  then []
  else match qhd(q) with
     Leaf          -> bfst(deq q)
   | (Node(l,v,r)) -> v::(bfst(enq(enq(deq(q),l),r)))

(* Tree balancing*)
(* Define the slope of a tree*)
let slope l =
  match l with
    Leaf -> 0
    | (Node (t1,a,t2)) -> (height t1) - (height t2)
(* If |slope s| <= 1 for every subtree, the tree is balanced*)

(* Rotating trees *)
let rot_left = function
  (Node (t1, a1, Node(t2, a2, t3))) -> Node (Node (t1, a1, t2), a2, t3)

let rot_right = function
  (Node (Node (t1, a1, t2), a2, t3)) -> (t1, a1, Node(t2, a2, t3))

(* Rebalance trees and shift *)
let rebal t =
  match (slope t) with
      2 -> shiftr t
    | -2 -> shiftl t
    | _ -> t

let shiftr = function
  (Node (t1, a, t2)) -> if (slope t1 = -1)
                        then rot_right (Node (rot_left t1, a, t2))
                        else rot_right (Node (t1, a, t2))
let shiftl = function
  (Node (t1, a, t2)) -> if (slope t2 = 1)
                        then rot_left (Node (t1, a, rot_right t2))
                        else rot_left (Node (t1, a, t2))
