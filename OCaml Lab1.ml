exception DoesNotExist
type attribute = string
type dcolumn = string * attribute list
type dtable = string * dcolumn list
type dbase = dtable list
type relalg = Project of (string list * string)
            | Rename of (string * string * string)
            | Restrict of (string * string * string)

(* General *)
let table = ("Student", [
   ("Id", ["2";"4";"7";"9"]);
   ("Name", ["Jim";"Linnea";"Steve";"Hannah"]);
   ("Gender",["Male";"Female";"Male";"Female"]);
   ("Course",["Geography";"Economics";"Informatics";"Geography"])
])

let dbase = ref [table]

let rec getColumn (dtable, attribute) =
  match dtable with
    (name, [])       -> raise DoesNotExist
  | (name, (a,b)::t) -> if a = attribute
                        then (a,b)
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
    []   -> []
  | h::t -> getColumn(dtable, h)::projection(t, dtable)

(* Rename *)
let rec replace (attOld, attNew, dcolumn) =
  match dcolumn with
    []         -> raise DoesNotExist
    | (a,b)::t -> if a = attOld
              then (attNew, b)::t
              else (a,b)::(replace(attOld, attNew, t))

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
    []         -> []
    | (a,b)::t -> (a, restrictAttrib(boolList, b))::(restrictColumns(boolList, t))

let restriction (attribute, a, dtable) =
  let boolList = generateBoolList(a, getColumn(dtable, attribute)) in
    match dtable with
      (name, columns) -> restrictColumns(boolList, columns)

(* Engine *)
let rec lookup (var, env) =
  match env with
    []               -> raise DoesNotExist
  | (tname,table)::t -> if var = tname
                          then (tname,table)
                          else lookup (var, t)

let eval (relalg, newtable, database) =
  match relalg with
       (Project(attribs, table))            -> let ntable = lookup(table, !database) in (newtable, projection(attribs, ntable))::(!dbase)
    |  (Rename (oldatrib, newatrib, table)) -> let ntable = lookup(table, !database) in (newtable, rename(oldatrib, newatrib, ntable))::(!dbase)
    |  (Restrict (atrib, value, table))     -> let ntable = lookup(table, !database) in (newtable, restriction(atrib, value, ntable))::(!dbase)
