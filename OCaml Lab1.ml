(*Lab 1 RelAlg*)

module type TABLE =
  sig

    type dtable
    type dcolumn
    type attribute
    val newDtable : dtable
    val newDcolumns : dcolumn
    val newAttributes : attribute

    val projection : attribute list * dtable -> dtable

    val rename : attribute * attribute * dtable -> dtable
    val replace : attribute * attribute * dtable -> dcolumn list

    val restriction : attribute * string * dtable -> dtable
    val restrictColumns : bool list * dcolumn list -> dcolumn list
    val restrictColumn : bool list * dcolumn -> dcolumn

    val generateBoolList : string * dcolumn list -> bool list
    val getColumn : dtable * attribute -> dcolumn

  end;;

module TinyRelationalAlgebra : TABLE =
  struct
    exception DoesNotExist

    type dtable = string * dcolumn list
    type dcolumn = string * attribute list
    type attribute = string

    let fst (a,b) = a
    let snd (a,b) = b
    (*generate basic table*)
    let newAttributes = []
    let newDcolumns = []
    let newDtable = []

(* PROJECTION *)

    let rec projection (attributes, dtable) =
      match attributes with
        []     -> ""
      | [h::t] -> getColumn(dtable, h); projection(t, dtable)

(* REPLACE *)

    let rename (attOld, attNew, dtable) =
      match dtable(name, dcolumn) with
        (name, dcolumn) -> (name, replace(attOld, attNew, dcolumn))

    let rec replace (attOld, attNew, dcolumn) =
      match dcolumn with
        []     -> raise DoesNotExist
      | [h::t] -> if fst(h) = attOld
                  then fst(h) <- attNew; h::(replace(attOld, attNew, (name, t))))
                  else h::(replace(attOld, attNew, (name, t)))

(* RESTRICTION *)

    let restriction (attribute, val, dtable) =
      let boolList = generateBoolList(val, getColumn(dtable, attribute)) in
        match dtable(name, columns) with
          (name, columns) -> (name, restrictColumns(boolList, columns))

    let rec restrictColumns (boolList, columns) =
      match columns with
        []     -> []
      | [h::t] -> restrictColumn(boolList, h)::(restrictColumns(boolList, t))

    let rec restrictColumn (boolList, column) =
      match (boolList, column) with
        ([], _)             -> []
      | ([h1::t1],[h2::t2]) -> if h1 = true
                               then h2::(restrictColumn(t1, t2))
                               else restrictColumn(t1, t2)

(* GENERAL FUNCTIONS *)
    (* GenerateBoolList *)
    (* generates a list of booleans.*)
    let rec generateBoolList (val, column) =
      match column(attribute, values) with
        (_ ,[])     -> []
      | (_,[h::t])  -> if val = h
                       then ["true"]::(generateBoolList(t))
                       else ["false"]::(generateBoolList(t))

    (* getColumn *)
    (* returns a column of a given attribute.*)
    let rec getColumn (dtable, attribute) =
      match dtable(name, columns) with
        (name, [])     -> raise DoesNotExist
      | (name, [h::t]) -> if fst(h) = attribute
                          then snd(h)
                          else getColumn((name, t), attribute)

end;;
