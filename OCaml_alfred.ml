exception DoesNotExist
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

(* General *)
let fst(a,b) = a

let snd(a,b) = b

let rec getColumn (dtable, attribute) =
  match dtable with
    (name, [])     -> raise DoesNotExist
  | (name, h::t) -> if fst(h) = attribute
                      then h
                      else getColumn((name, t), attribute)

let rec generateBoolList (a, column) =
  match column with
    (_,[])     -> []
    | (_,h::t) -> if a = h
                   then "true"::(generateBoolList(a , ("temp", t)))
                   else "false"::(generateBoolList(a , ("temp",t)))

(* Projection *)
let rec projection (attribute, dtable) =
  match attribute with
    []     -> []
  | h::t -> getColumn(dtable, h)::projection(t, dtable)

(* Rename *)
let rec replace (attOld, attNew, dcolumn) =
  match dcolumn with
    []     -> raise DoesNotExist
    | h::t -> if fst(h) = attOld
              then (attNew, snd(h))::t
              else h::(replace(attOld, attNew, t))

let rename (attOld, attNew, dtable) =
  match dtable with
    (name, dcolumn) -> replace(attOld, attNew, dcolumn)

(* Restrict *)
let rec restrictAttrib (boolList, column) =
  match (boolList, column) with
    ([], _)          -> []
  | (_, [])          -> []
  | (h1::t1, h2::t2) -> if h1 = "true"
                        then h2::(restrictAttrib(t1, t2))
                        else restrictAttrib(t1, t2)

let rec restrictColumns (boolList, columns) =
  match columns with
    []     -> []
    | h::t -> (fst(h), restrictAttrib(boolList, snd(h)))::(restrictColumns(boolList, t))

let restriction (attribute, a, dtable) =
  let boolList = generateBoolList(a, getColumn(dtable, attribute)) in
    match dtable with
      (name, columns) -> restrictColumns(boolList, columns)

(* Engine *)
let eval (q, newtable, database) =
  match q with
    (Project (attribs, table)) -> (newtable, projection(attribs, database))
    |  (Rename (oldatrib, newatrib, table)) -> (newtable, rename(oldatrib, newatrib, database))
    |  (Restrict (atrib, value, table)) -> (newtable, restriction(atrib, value, database))
