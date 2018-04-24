(* Game Player Data Types and Functions *)
exception CannotPlacePiece;;
type piece = X | O | Empty;;
type board = (piece list) list;;
type game = NillGame | Level of (board * game list);;
type status = X_wins | O_wins | Draw | Unfinished;;
type move_status = Updated of board | NotAllowed;;

let table = [[Empty;Empty;Empty];[Empty;Empty;Empty];[Empty;Empty;Empty]];;

(* make_move takes a player's piece, the position that they want to put in on and the board *)
(* make_move returns an updated board with that piece in the position if possible, does nothing otherwise *)

let make_move piece (x,y) board =
   let rec walk_down i board =
            match board with
              []     -> raise CannotPlacePiece
            | (h::t) ->
		if i=1
		then ([],h,t)
		else
		    let (pre,row,rest) = walk_down (i-1) t
		    in (h::pre,row,rest)   in
   let rec walk_acc j row =
	    match row with
	      []     -> raise CannotPlacePiece
	    | (h::t) ->
		if j=1
		then
		   match h with
		      Empty -> ([],piece,t)
                   |  _     -> ([],h,t)
		else
		   let (pre,item,rest) = walk_acc (j-1) t
		   in (h::pre,item,rest)    in
   let (prerows,r,postrows) = walk_down x board in
   let (pcol,c,postcols) = walk_acc y r in
   let newrow = pcol @ (c::postcols) in
   let board2 = prerows @ (newrow::postrows)
   in 	if board=board2
	then NotAllowed
	else Updated board2;;



(* gen_moves takes a piece and a board *)
(* It returns a list of boards in which that piece has been placed *)

let gen_moves piece board =
    let rec place_cols (i,j) =
	if j<=3
	then (make_move piece (i,j) board)::(place_cols (i,j+1))
	else [] in
    let rec place_rows i =
	if i<=3
	then (place_cols (i,1)) @ (place_rows (i+1))
	else [] in
    let rec filter_moves bds =
	match bds with
	  []                 -> []
        | ((Updated b)::rst) -> b::(filter_moves rst)
        | (NotAllowed::rst)  -> filter_moves rst   in
    let boards = place_rows 1
    in filter_moves boards;;

type status = X_wins | O_wins | Draw | Unfinished;;

(* is_EmptyPos returns true if there is an Empty position on the board *)

let is_EmptyPos board =
    let rec check_row r =
	match r with
	  []         -> false
	| (Empty::t) -> true
	| (_::t)     -> check_row t  in
    let rec check_rows b =
	match b with
	  []     -> false
	| (h::t) -> (check_row h) || check_rows t
    in check_rows board;;

(* is_win takes a piece and a board and returns true if that piece does indeed have three in a row *)

let is_win piece board =
    let id x = x in
    let add1 x = x+1 in
    let sub1 x = x-1 in
    let rec check_row r =
	match r with
	  []      -> true
        | (h::t)  -> (h=piece) && check_row t  in
    let rec is_row rs =
	match rs with
	  []      -> false
	| (h::t)  -> (check_row h) || is_row t in
    let rec test_row pos r =
	match (pos,r) with
	  (p,[])      -> false
	| (1,(h::t))  -> (h=piece)
	| (n,(h::t))  -> test_row (n-1) t in
    let rec is_column pos f b =
	match b with
	  []      -> true
	| (h::t)  -> test_row pos h && is_column (f pos) f t in
    let is_col b = (is_column 1 id b) || (is_column 2 id b) || (is_column 3 id b) in
    let is_left_diag b = is_column 1 add1 b in
    let is_right_diag b = is_column 3 sub1 b in
    let is_diag b = is_left_diag b || is_right_diag b
    in is_row board || is_col board || is_diag board;;


(* calc_status takes a board and returns the status of the board *)

let calc_status board =
	if (is_win X board)
	then X_wins
	else (if (is_win O board)
	      then O_wins
	      else (if (is_EmptyPos board)
                    then Unfinished
                    else Draw));;

let completed bd = not ((calc_status bd) = Unfinished);;

let swap_piece p =
    match p with
      X -> O
    | O -> X;;

(* gen_games generates all the games from the current board with piece moving first *)

let rec gen_games piece bd =
	if completed bd
	then
	   Level (bd,[])
	else
	   let ms = gen_moves piece bd in
	   let gs = gen_nextlevel piece ms
	   in Level (bd,gs)

and gen_nextlevel piece ms =
	match ms with
	  []      -> []
	| (b::bs) ->
		let g = gen_games (swap_piece piece) b
		in g::(gen_nextlevel piece bs);;

(* gen_limited_games generates all the games to the given depth *)

let rec gen_limited_games piece bd depth =
	if (completed bd) || (depth=0)
	then
	   Level (bd,[])
	else
	   let ms = gen_moves piece bd in
	   let gs = gen_next_limited_level piece ms (depth-1)
	   in Level (bd,gs)

and gen_next_limited_level piece ms depth =
	match ms with
	  []      -> []
	| (b::bs) ->
		let g = gen_limited_games (swap_piece piece) b depth
		in g::(gen_next_limited_level piece bs depth);;

(* extra test case. *)
let game = [[O; Empty; Empty]; [O; X; Empty]; [X; O; X]];;

(* Task A: *)
(* Function "searchWin" takes a game tree and a piece.
   Returns true or false depending on if the given piece has a chance to win on the given game. *)
let rec searchWin gt piece =
  match gt with
    NillGame      -> false
  | Level (b,[])  ->
    (match (calc_status b,piece) with
      (X_wins,X)     -> true
    | (O_wins,O)     -> true
    | _              -> false)
  | Level (b, gl) -> loop gl piece
and loop gl piece =
  match gl with
    []   -> false
  | h::t -> (searchWin h piece) || (loop t piece);;

(* Example: *)
searchWin (gen_games X game) X;;

(* Task B: *)
(* Function "winTree" takes a game tree and a piece.
   Returns a given tree of moves for the first winning play in the given game table. *)
let rec winTree gt piece =
  match gt with
    NillGame -> []
  | Level (b,[]) ->
    (match (calc_status b,piece) with
      (X_wins,X)     -> [b]
    | (O_wins,O)     -> [b]
    | _              -> [])
  | Level (b,gl::t) -> if winTree gl piece = []
                       then winTree (Level(b,t)) piece
                       else b::winTree gl piece;;

(* Example: *)
winTree (gen_games X game) X;;

(*Task C: *)
(* Function "addToFront" takes a board and a list of boards.
   Returns an updated list with the board added to the front of the list of boards. *)
let rec addToFront move l =
    match l with
        []    -> [move]
    |   h::[] -> [move @ h]
    |   h::t  -> (move @ h) :: addToFront move t;;

(* Function "allWinTree" takes a game tree and a piece.
   Returns a list of lists of winning moves. *)
let rec allWinTree gt piece =
    match gt with
        NillGame         -> []
    |   Level(board, []) -> (
      match (calc_status board, piece) with
        (X_wins,X)     -> [[board]]
      | (O_wins,O)     -> [[board]]
      | _              -> [])
    |   Level(board, h::t) -> if allWinTree h piece = []
                                 then allWinTree h piece @ allWinTree (Level(board, t)) piece
                                 else addToFront [board] (allWinTree h piece) @ allWinTree (Level(board, t)) piece;;

(* Example: *)
allWinTree (gen_games X game) X;;

(*Task D: *)
(* Function "shortest" takes a list of lists of winning moves generated by task C.
   Returns the shortest list of winning moves. *)
let rec shortest movelist =
  match movelist with
    []    -> []
  | h::[] -> h
  | h::t  -> if List.length h < List.length (shortest t)
             then h
             else shortest t;;

(* Example: *)
shortest (allWinTree (gen_games X game) X);;

(*Task E: *)
(* Support functions *)
let addWins (x1,o1,d1) (x2,o2,d2) = (x1+x2,o1+o2,d1+d2);;
let snd (a,b) = b;;

(* Function "numberOfWins" takes a game tree and a tuple.
   Returns a tuple filled with information on terminal node win/lose/draw count. *)
let rec numberOfWins game (x,o,d) =
    match game with
        NillGame           -> (0,0,0)
    |   Level(current, []) -> (match calc_status current with
                                   Unfinished -> (x,o,d)
                               |   Draw       -> (x,o,d+1)
                               |   X_wins     -> (x+1,o,d)
                               |   O_wins     -> (x,o+1,d))
    |   Level(current, next::rest) -> addWins (numberOfWins next (x,o,d)) (numberOfWins (Level(current, rest)) (x,o,d));;

(* Function "calcPlayer" takes a piece and a tuple.
   Returns a float value. *)
let calcPlayer piece (x,o,d) =
    match piece with
        Empty -> 0.0
    |   X     -> if x > 0 then float_of_int x /. float_of_int (x+o+d) else 0.0
    |   O     -> if o > 0 then float_of_int o /. float_of_int (o+x+d) else 0.0;;

(* Function "getOdds" takes a piece and a game tree.
   Returns a list of gamemoves (boards of the next move available) and a float. *)
let rec getOdds piece game =
    match game with
        NillGame                   -> []
    |   Level(current, [])         -> []
    |   Level(current, NillGame::_) -> []
    |   Level(current, Level(b,l)::rest) -> (b, (calcPlayer piece (numberOfWins (Level(b,l)) (0,0,0)) ) *. 100.0) :: getOdds piece (Level(current, rest));;

(* Function "getHighest" takes a list of gamemoves.
   Returns the board and float value of the element in the list containing the highest float value. *)
let rec getHighest l =
  match l with
    []    -> ([],0.0)
  | (a,b)::[] -> (a,b)
  | (a,b)::t  -> if b > snd (getHighest t)
                 then (a,b)
                 else getHighest t;;

(* Function "recommendOddsMove" takes a game tree and a piece.
   Returns a board wich has the highest winning chance for that specified player. *)
let recommendOddsMove game piece=
  let oddsList = getOdds piece game in
    match (getHighest oddsList) with
      a,b -> a;;

(* Example: *)
recommendOddsMove (gen_games X game) X;;


(* Alternative solution Task E: (Using task C).
   _WARNING_  slow and complicated method. *)
(* Function "terminal_nodes" takes a game tree and returns the number of terminal nodes. *)
let rec terminal_nodes gt =
  match gt with
    NillGame -> 0
    | Level (b , [])   -> 1
    | Level (b , h::t) -> terminal_nodes h + loop t
and loop gl =
  match gl with
        []   -> 0
      | h::t -> terminal_nodes h  + loop t;;

(* Function "ratio" takes a list of nodes and a piece.
   Returns a list with a single board in it.*)
let rec ratio nodelist piece =
  match nodelist with
    []   -> []
  | hg::t ->  match hg with
                  NillGame -> []
                | Level (b , []) -> (match (calc_status b,piece) with
                                      (X_wins,X)     -> [b]
                                    | (O_wins,O)     -> [b]
                                    | _              -> [])
                | Level (b , h::ht) -> (if terminal_nodes (gen_games piece b) > 0 && List.length (allWinTree hg piece) > 0
                                        then if float_of_int (List.length (allWinTree hg piece)) /. float_of_int (terminal_nodes (gen_games piece b)) >= float_of_int (List.length (ratio t piece))
                                             then [b]
                                             else ratio t piece
                                        else ratio t piece );;

(* Function "recommendMove" takes a game tree and a piece.
   Returns a board with the single best move for a player to make. *)
let recommendMove gt piece =
  match gt with
    NillGame        -> []
  | Level (b, [])   -> []
  | Level (b, h::t) -> ratio (h::t) piece;;

(* Example: *)
recommendMove (gen_games X game) X;;
