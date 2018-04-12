exception ColumnDoesNotExist
exception TableDoesNotExist
type attribute = string
type dcolumn = string * attribute list
type dtable = string * dcolumn list
type relalg = Project of (string list * string)
            | Rename of (string * string * string)
            | Restrict of (string * string * string)

let table = ("Student", [
   ("Id", ["2";"4";"7";"9"]);
   ("Name", ["Jim";"Linnea";"Steve";"Hannah"]);
   ("Gender",["Male";"Female";"Male";"Female"]);
   ("Course",["Geography";"Economics";"Informatics";"Geography"])
])

let fst(a,b) = a
let snd(a,b) = b


(* Projection *)
let rec projection (attribute, dtable) =
  match attribute with
    []     -> []
  | h::t -> getColumn(dtable, h)::projection(t, dtable)

(* Rename *)
let rename (attOld, attNew, dtable) =
  match dtable with
    (name, dcolumn) -> (name, replace(attOld, attNew, dcolumn))

let rec replace (attOld, attNew, dcolumn) =
  match dcolumn with
    []     -> raise ColumnDoesNotExist
    | h::t -> if fst(h) = attOld
              then (attNew, snd(h))::t
              else h::(replace(attOld, attNew, t))

(* Restrict *)
let restriction (attribute, a, dtable) =
  let boolList = generateBoolList(a, getColumn(dtable, attribute)) in
    match dtable with
      (name, columns) -> (name, restrictColumns(boolList, columns))

let rec restrictColumns (boolList, columns) =
  match columns with
    []     -> []
    | h::t -> (fst(h), restrictAttrib(boolList, snd(h)))::(restrictColumns(boolList, t))

let rec restrictAttrib (boolList, column) =
  match (boolList, column) with
    ([], _)          -> []
  | (_, [])          -> []
  | (h1::t1, h2::t2) -> if h1 = "true"
                        then h2::(restrictAttrib(t1, t2))
                        else restrictAttrib(t1, t2)

(* General *)
let rec getColumn (dtable, attribute) =
  match dtable with
    (name, [])     -> raise ColumnDoesNotExist
  | (name, h::t) -> if fst(h) = attribute
                      then h
                      else getColumn((name, t), attribute)

let rec generateBoolList (a, column) =
  match column with
    (_,[])     -> []
    | (_,h::t) -> if a = h
                   then "true"::(generateBoolList(a , ("temp", t)))
                   else "false"::(generateBoolList(a , ("temp",t)))

(* Engine *)
let eval (q, newtable, database) =
  match q with
    (Project (attribs, table))   -> projection(attribs, table)
    |  (Rename (oldatrib, newatrib, table)) -> rename(oldatrib, newatrib, table)
    |  (Restrict (atrib, val, table)) -> restriction(atrib,val,table)
