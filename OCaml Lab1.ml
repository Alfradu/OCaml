exception DoesNotExist;;
type attribute = string;;
type dcolumn = string * attribute list;;
type dtable = string * dcolumn list;;
type dbase = dtable list;;

(* Type declaration for Task 2 TRA*)
type relalg = Project of (string list * string)
            | Rename of (string * string * string)
            | Restrict of (string * string * string);;

let table = ("Student", [
   ("Id", ["2";"4";"7";"9"]);
   ("Name", ["Jim";"Linnea";"Steve";"Hannah"]);
   ("Gender",["Male";"Female";"Male";"Female"]);
   ("Course",["Geography";"Economics";"Informatics";"Geography"])]);;

let dbase = ref [table];;

(* Task 1: Tiny relational algebra(TRA) expression functionality. *)

(* Function "getColumn" takes a table and a string.
   Returns a column with that string name. *)
let rec getColumn (dtable, attribute) =
  match dtable with
    (name, [])       -> raise DoesNotExist
  | (name, (a,b)::t) -> if a = attribute
                        then (a,b)
                        else getColumn((name, t), attribute);;

(* Function "generateBoolList" takes a value and a column.
   Returns a generated list of string "booleans" for it. *)
let rec generateBoolList (a, column) =
  match column with
    (_,[])     -> []
    | (_,h::t) -> if a = h
                   then "true"::(generateBoolList(a , ("temp", t)))
                   else "false"::(generateBoolList(a , ("temp",t)));;

(* Projection *)
(* Function "projection" takes a list of string and a table.
   Returns the provided attribute column. *)
let rec projection (attribute, dtable) =
  match attribute with
    []   -> []
  | h::t -> getColumn(dtable, h)::projection(t, dtable);;

(* Rename *)
(* Function "replace" takes a string(Old attribute), a string(New attribute), and a column.
   Returns a table with an updated column name. *)
let rec replace (attOld, attNew, dcolumn) =
  match dcolumn with
    []         -> raise DoesNotExist
    | (a,b)::t -> if a = attOld
              then (attNew, b)::t
              else (a,b)::(replace(attOld, attNew, t));;

(* Function "rename" takes a string(Old attribute), a string(New attribute), and a table.
   Returns a new table with a name(dtable) where a column has a "attNew" name. *)
let rename (attOld, attNew, dtable) =
  match dtable with
    (name, dcolumn) -> replace(attOld, attNew, dcolumn);;

(* Restriction *)
(* Function "restrictAttrib" takes a string "boolList" and a column.
   Returns attributes from a column where boolList = "true" *)
let rec restrictAttrib (boolList, column) =
  match (boolList, column) with
    ([], _)          -> []
  | (_, [])          -> []
  | (h1::t1, h2::t2) -> if h1 = "true"
                        then h2::(restrictAttrib(t1, t2))
                        else restrictAttrib(t1, t2);;

(* Function "restrictColumns" takes a string "boolList" and a list of columns.
   Returns a list of columns. *)
let rec restrictColumns (boolList, columns) =
  match columns with
    []         -> []
    | (a,b)::t -> (a, restrictAttrib(boolList, b))::(restrictColumns(boolList, t));;

(* Function "restriction" takes an attribute, a value, and a table.
   Returns a new table with only unrestricted values. *)
let restriction (attribute, a, dtable) =
  let boolList = generateBoolList(a, getColumn(dtable, attribute)) in
    match dtable with
      (name, columns) -> restrictColumns(boolList, columns);;

(* Examples: *)
projection (["Name";"Gender";"Id"], table);;
rename ("Name", "FirstName", table);;
restriction ("Gender", "Male", table);;

(* Task 2: Relational algebra engine. *)

(* Function "lookup" takes a string and a database.
   Returns table of specified name. *)
let rec lookup (var, env) =
  match env with
    []               -> raise DoesNotExist
  | (tname,table)::t -> if var = tname
                          then (tname,table)
                          else lookup (var, t);;

(* Function "eval" takes a relational algebra expression, a table and a database.
   Returns a new database with the performed relalg expression applied to the new table. *)
let eval (relalg, newtable, database) =
  match relalg with
       (Project(attribs, table))            -> let ntable = lookup(table, !database) in dbase := (newtable, projection(attribs, ntable))::(!dbase); !dbase
    |  (Rename (oldatrib, newatrib, table)) -> let ntable = lookup(table, !database) in dbase := (newtable, rename(oldatrib, newatrib, ntable))::(!dbase); !dbase
    |  (Restrict (atrib, value, table))     -> let ntable = lookup(table, !database) in dbase := (newtable, restriction(atrib, value, ntable))::(!dbase); !dbase;;

(* Example: *)
eval (Project(["Name";"Gender"], "Student"), "NewTable", dbase);;
eval (Rename("Name", "FirstName", "Student"), "RenamedStudent", dbase);;
eval (Restrict("Gender", "Female", "RenamedStudent"), "FemaleStudents", dbase);;
