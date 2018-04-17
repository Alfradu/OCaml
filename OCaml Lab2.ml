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

    (* Task a: Write a function that takes a game tree and a player,
    and returns true if that player can win the game.
    This function should perform a depth-first search and
    look at terminal nodes using calc_status. *)

let rec search_win gt piece =
  match gt with
    NillGame      -> false
  | Level (b,[])  ->
    (match (calc_status b,piece) with
      (X_wins,X)     -> true
    | (O_wins,O)     -> true
    | _              -> false)
  | Level (b, gl) -> loop_through gl piece
and loop_through gl piece =
  match gl with
    []   -> false
  | h::t -> (search_win h piece) || (loop_through t piece);;

(* Task b: Write a function that takes a game tree and a player,
and returns a winning list of moves for that player.
This function will perform a depth-first search using your
solution to task a and return a single list of boards that
yields a win for the given player. *)

let rec win_tree gt piece =
  match gt with
    NillGame -> []
  | Level (b,[]) ->
    (match (calc_status b,piece) with
      (X_wins,X)     -> [b]
    | (O_wins,O)     -> [b]
    | _              -> [])
  | Level (b,gl::t) -> if search_win gl piece
                       then [b] @ (win_tree gl piece)
                       else loop t piece
and loop gl piece =
  match gl with
    []   -> []
  | gt::t -> if search_win gt piece
            then win_tree gt piece
            else loop t piece

(*Task c: Write a function that returns all winning lists of
moves for a given player and a board. *)

let rec win_all gt piece =
  match gt with
    NillGame -> []
  | Level (b , []) -> []
  | Level (b , h::t) -> if search_win gt piece
                        then [b::(win_tree gt piece)] @ win_all (Level(b,t)) piece
                        else win_all (Level(b,t)) piece

(*Task d: Use the result of task c to find the shortest winning
game possible for a given player. *)

(*Task e: Deduce the best move that a given player should take on a given board.
In other words, which move is most likely to get a win compared to others. *)
