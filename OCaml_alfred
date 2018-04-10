exception ColumnDoesNotExist
exception TableDoesNotExist
type attribute = string
type attributes = attribute list
type dcolumn = string * attributes
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
let rec projection (attributes, dtable) =
  match attributes with
    []     -> dtable
  | [h::t] -> getColumn(dtable, h); projection([t], dtable)


(* Rename *)
let rec rename (attOld, attNew, dtable) =
  match dtable(name, dcolumn) with
    none      -> raise TableDoesNotExist
  | (_,[])     -> dtable
  | (_,[h::t]) -> if fst(h) = attOld
                     then fst(h) = attNew; h::rename(attOld, attNew, (name, t))
                     else h::rename(attOld, attNew, (name, t))


(* Restrict *)
let restriction (attribute, val, dtable) =
  let boolList = generateBoolList(val, getColumn(dtable, attribute)) in
    match dtable(name, columns) with
      (_, columns) -> (name, restrictColumns(boolList, columns))
let rec restrictColumns (boolList, columns) =
  match columns with
    []     -> []
  | [h::t] -> restrictColumn(boolList, [h])::(restrictColumns(boolList, [t]))
let rec restrictColumn (boolList, column) =
  match (boolList, column) with
    ([], _)             -> []
  | ([h1::t1],[h2::t2]) -> if h1 = true
                           then h2::(restrictColumn([t1], [t2]))
                           else restrictColumn([t1], [t2])


(* Engine *)
let eval (q, newtable, database) =
    match q with
       (Project (attribs, table))   -> projection(attribs, table)
    |  (Rename (oldatrib, newatrib, table)) -> rename(oldatrib, newatrib, table)
    |  (Restrict (atrib, val, table)) -> restriction(atrib,val,table)


(* General *)
let rec getColumn (dtable, attribute) =
  match dtable with
    (name, [])     -> raise ColumnDoesNotExist
  | (name, [h::t]) -> if fst(h) = attribute
                      then snd(h)
                      else getColumn((name, [t]), attribute)

let rec generateBoolList (val, column) =
  match (val, column) with
      (_ ,[])     -> []
    | (_,[h::t])  -> if val = h
                     then ["true"]::(generateBoolList(val ,t))
                     else ["false"]::(generateBoolList(val ,t))
