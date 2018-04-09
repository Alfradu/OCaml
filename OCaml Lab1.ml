(*Lab 1 RelAlg*)

module type TABLE =
  sig
    type attribute
    type dcolumn
    type dtable
    val newAttributes : attribute
    val newDcolumns : dcolumn
    val newDtable : dtable
    val projection : attributes * dtable -> dtable
    val renaming : attribute * attribute * dtable -> dtable
    val restriction : attribute * string * dtable -> dtable
  end;;

module TinyRelationalAlgebra : TABLE =
  struct
    exception DoesNotExist

    type attributes = attribute list
    type attribute = string
    type dcolumn = attribute * string list
    type dtable = string * dcolumn list
    type

    let fst (a,b) = a
    let snd (a,b) = b

    (*generate basic table*)
    let newAttributes = [|"id";"gender";"name";"course"|]
    let newDcolumns = (*Gå igenom attributes-listan och lägg ihop med en egen lista av innehåll till de attributerna*)
    let newDtable = (*Sätt ihop columnlistan med ett namn*)

(*SUPERSOLUTION, WOULD HAVE WORKED 10/10
    let ghettoMorph (dtable, func, attributes) =
      let name = fst(dtable) in
        match func with
          "projection"   -> (name, projection(dtable, attribute))
        | "rename"       -> (name, renaming())
        | "restriction"  -> (name, restriction())
        |  _             -> ("YOU FOUND THE EASTER EGG HAHHAHAHAHAHA", []) is actually not good do not use
*)
    let rec getColumn (dtable, attribute) =
      match dtable(name, columns) with
        (name, [])     -> raise DoesNotExist
      | (name, [h::t]) -> if fst(h) = attribute
                          then snd(h)
                          else getColumn((name, t), attribute)

    let rec projection (attributes, dtable) =
      match attributes with
        []     -> ""
      | [h::t] -> getColumn(h); projection(t, dtable)

    let replace (attOld, attNew, dtable) =
      match dtable(name, dcolumn) with
        (name, _) -> (name, renaming(attOld, attNew, dtable))

    let rec renaming (attOld, attNew, dtable) =
      match dtable(name, dcolumn) with
        (name, [])     -> raise DoesNotExist
      | (name, [h::t]) -> if fst(h) = attOld
                          then fst(h) <- attNew; h::(renaming(attOld, attNew, (name, t))))
                          else h::(renaming(attOld, attNew, (name, t)))

    let restriction (attribute, val, dtable) =
      let column = getColumn(dtable, attribute) in
        match dtable(name, columns) with
          (name, _) -> (name, restrict(val, current))

    let rec restrict (val, current) =


      match dtable(name, dcolumn) with
        (name, [])     -> raise DoesNotExist
      | (name, [h::t]) ->
end;;
