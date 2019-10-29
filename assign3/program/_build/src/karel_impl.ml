open Core
open Option.Monad_infix

exception Unimplemented

(* Set this to true to print out intermediate state between Karel steps *)
let debug = false

type cell =
  | Empty
  | Wall
  | Beeper

type grid = cell list list

type dir =
  | North
  | West
  | South
  | East

type pos = int * int

type state = {
  karel_pos : pos;
  karel_dir : dir;
  grid : grid;
}

let get_cell (grid : grid) ((i, j) : pos) : cell option =
  (List.nth grid j) >>= fun l -> List.nth l i
;;

let set_cell (grid : grid) ((i, j) : pos) (cell : cell) : grid =
  List.mapi grid ~f:(fun j' l ->
    if j = j' then List.mapi l ~f:(fun i' c -> if i = i' then cell else c)
    else l)
;;

let state_to_string (state : state) : string =
  String.concat ~sep:"\n" (List.mapi state.grid ~f:(fun (row_idx : int) (row : cell list) : string ->
      String.concat ~sep:" " (List.mapi row ~f:(fun (col_idx : int) (col: cell) : string ->
        if (col_idx, row_idx) = state.karel_pos then
          "K"
        else
          match col with
          | Empty -> "."
          | Wall -> "x"
          | Beeper -> "B"
        )
      )
    )
  )
;;

let empty_grid (m : int) (n : int) : grid =
  List.map (List.range 0 m) ~f:(fun _ ->
    List.map (List.range 0 n) ~f:(fun _ -> Empty))
;;

type predicate =
  | FrontIs of cell
  | NoBeepersPresent
  | Facing of dir
  | Not of predicate

type instruction =
  | Move
  | TurnLeft
  | PickBeeper
  | PutBeeper
  | While of predicate * instruction list
  | If of predicate * instruction list * instruction list

let rec predicate_to_string (pred : predicate) : string =
  match pred with
  | FrontIs c ->
    let cellstr = match c with
      | Empty -> "Empty" | Beeper -> "Beeper" | Wall -> "Wall"
    in
    Printf.sprintf "FrontIs(%s)" cellstr
  | NoBeepersPresent -> "NoBeepersPresent"
  | Facing dir ->
    let dirstr = match dir with
      | North -> "North" | South -> "South" | East -> "East" | West -> "West"
    in
    Printf.sprintf "Facing(%s)" dirstr
  | Not pred' -> Printf.sprintf "Not(%s)" (predicate_to_string pred')

let rec instruction_to_string (instr : instruction) : string =
  match instr with
  | Move -> "Move"
  | TurnLeft -> "TurnLeft"
  | PickBeeper -> "PickBeeper"
  | PutBeeper -> "PutBeeper"
  | While (pred, instrs) ->
    Printf.sprintf "While(%s, [%s])"
      (predicate_to_string pred)
      (instruction_list_to_string instrs)
  | If (pred, then_, else_) ->
    Printf.sprintf "If(%s, [%s], [%s])"
      (predicate_to_string pred)
      (instruction_list_to_string then_)
      (instruction_list_to_string else_)
and instruction_list_to_string (instrs: instruction list) : string =
  String.concat ~sep:", " (List.map ~f:instruction_to_string instrs)

let front_pos (state : state) : pos =
  let (x, y) = state.karel_pos in
    match state.karel_dir with
    | North -> (x, y - 1)
    | West -> (x - 1, y)
    | South -> (x, y + 1)
    | East -> (x + 1, y)

let beeper_free_row (row : cell list) : bool =
  (List.length (List.filter row ~f:(fun (col : cell) : bool -> col = Beeper))) = 0

let rec eval_pred (state : state) (pred : predicate) : bool =
  match pred with
  | FrontIs cell -> (match (get_cell state.grid (front_pos state)) with
    | None -> cell = Wall
    | Some ctype -> cell = ctype)
  | NoBeepersPresent -> (match (get_cell state.grid state.karel_pos) with
    | None -> assert false; true
    | Some ctype -> ctype <> Beeper)
  | Facing direction -> direction = state.karel_dir
  | Not prednot -> not (eval_pred state prednot)

let move_forward (state : state) : state =
  if eval_pred state (FrontIs Wall) then
    state
  else
    {state with karel_pos = (front_pos state)}

let turn_left (state : state) : state =
  match state.karel_dir with
  | North -> {state with karel_dir = West}
  | West -> {state with karel_dir = South}
  | South -> {state with karel_dir = East}
  | East -> {state with karel_dir = North}

let pick_beeper (state : state) : state =
  if not (eval_pred state NoBeepersPresent) then
    {state with grid = (set_cell state.grid state.karel_pos Empty)}
  else
    state

let put_beeper (state : state) : state =
  if eval_pred state NoBeepersPresent then
    {state with grid = (set_cell state.grid state.karel_pos Beeper)}
  else
    state

(* let s : state = {
  karel_pos = (0, 1);
  karel_dir = North;
  grid = [[Empty; Empty; Wall]; [Empty; Wall; Wall]; [Empty; Beeper; Empty]]
} *)

let rec step (state : state) (code : instruction) : state =
  match code with
  | Move -> move_forward state
  | TurnLeft -> turn_left state
  | PickBeeper -> pick_beeper state
  | PutBeeper -> put_beeper state
  | While (pred, code_list) -> if (eval_pred state pred) then step (step_list state code_list) code else state
  | If (pred, true_code, else_code) -> if (eval_pred state pred) then step_list state true_code else step_list state else_code

and step_list (state : state) (instrs : instruction list) : state =
  List.fold instrs ~init:state ~f:(fun state instr ->
    if debug then
       (Printf.printf "Executing instruction %s...\n"
          (instruction_to_string instr);
        let state' = step state instr in
        Printf.printf "Executed instruction %s. New state:\n%s\n"
          (instruction_to_string instr)
          (state_to_string state');
        state')
     else
       step state instr)

;;

let checkers_algo : instruction list = [While(Not(FrontIs(Wall)), [PutBeeper; Move; If(FrontIs(Wall), [Move], [PutBeeper; Move; PutBeeper])]);
                                        While(Not(Facing(West)), [TurnLeft]);
                                        If(NoBeepersPresent, [Move], [PutBeeper]);
                                        While(Not(FrontIs(Wall)), [Move; PickBeeper; Move]);
                                        While(Not(Facing(South)), [TurnLeft]);
                                        While(Not(FrontIs(Wall)), [If(NoBeepersPresent, [Move; While(Not(Facing(East)), [TurnLeft]); PutBeeper],
                                                                                        [Move; While(Not(Facing(East)), [TurnLeft]); Move; PutBeeper]);
                                                                   While(Not(FrontIs(Wall)), [PutBeeper; Move; If(FrontIs(Wall), [Move], [PutBeeper; Move; PutBeeper])]);
                                                                   While(Not(Facing(West)), [TurnLeft]);
                                                                   If(NoBeepersPresent, [Move], [PutBeeper]);
                                                                   While(Not(FrontIs(Wall)), [Move; PickBeeper; Move]);
                                                                   While(Not(Facing(South)), [TurnLeft])]
                                              )
]



