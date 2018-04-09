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
    let rec add_entry (person, number, book) =
      match book with
      [] -> [(person, number)]
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

module ArrayPhoneBook : PHONEBOOK =
  struct
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
