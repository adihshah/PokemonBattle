open Battle
(** 
   Representation of dynamic battle state.

   This module represents the state of a battle as it is being played,
   including whose turn it is, each player's hp, information about the battle, 
   and functions that cause the state to change.
*)

(*********************************************************************)
(** The type of a move's current state in the game to reflect move-changes *)
type state_m = {id: string; mutable pp: int; attack: int; move_type: string; 
                mov: move}

(** The type of a pokemon's current state in the game to reflect poke-changes *)
type state_p = {id: string; mutable hp: int; mutable atk: int; mutable def: int; 
                mutable spatk: int; mutable spdef: int;
                moves: state_m list; front: string; back: string; desc: string; 
                pok: pokemon}

(** The type of an item's current state in the game to reflect item-changes *)
type state_i = {id: string; mutable uses: int; value: float; itemtype: string}

(** The abstract type of values representing the game state. *)
type t = {battle: Battle.t; p1_party: state_p list; p2_party: state_p list; 
          turn: int;
          last_move_used: state_m option; p1_items: state_i list; p2_items: 
            state_i list;
          p1_active_id: string; p2_active_id: string; mutable missed: bool}

(** The type of a Legal or Illegal state *)
type result = Legal of t | Illegal | Out

(** The type of a Legal or Illegal move *)
type m = Megal of state_m | Immegal

(** [init_state battle] is the initial state of the game 
    when playing battle [battle]. In that state the players 
    each have maximum hp and it is player 1's turn. *)
val init_state : Battle.t -> t

(** [effectiveness move_type target_type] returns a multiplier 0.5, 1.0, or 2.0,
    depending on how effective [move_type] is against [target_type] *)
val effectiveness: string -> string -> float

(** [get_move state num move] Returns data type move with name corresponding 
    to the concatenated string list [move]*)
val get_move: t -> int -> string list -> m

(** [attack battle moves] returns a new state where the player whose turn it
    is attacks the other player, subtracting from their hp the value of the 
    attack of the move used. Returns Illegal if the move was not valid. *)
val attack: t -> string list -> result

(** [get_last_move_used] returns the type of the last move used in the battle. 
*)
val get_last_move_used: t -> string

(** [get_turn state] Given a state, returns 1 if it is player 1's turn, 
    2 if it is player 2's turn. *)
val get_turn: t -> int

(** [get_hp state num] Given a state and a player 1 or 2, returns their 
    current hp *)
val get_hp: t -> int -> int

(** [use state item] Given a [state] and [item], mutates the state to reflect 
    the fact that the [item] was used. *)
val use: t -> string list -> result

(** [swap state name] Given a [state] and pokemon [name], mutates the state to 
    reflect the fact that the pokemon whose name is [name] was swapped with 
    current pokemon. *)
val swap: t -> string list -> result

(** [get_statep_from_name name lst] Given a pokemon [name] and state_p [lst], 
    is the [state_p] whose [id] is [name] *)
val get_statep_from_name: string -> state_p list -> state_p

(** [calculate_dmg state state_move state_user state_target] Given the game's
    [state], the declared [state_move] to be used, the [state_user] 
    who casted the move, and the [state_target] of the move, calculates the 
    damage dealt using the official formula from the Pokemon games. *)
val calculate_dmg: t -> state_m -> state_p -> state_p -> float