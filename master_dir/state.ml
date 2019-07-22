open Battle

type state_m = {id: string; mutable pp: int; attack: int; move_type: string;
                mov: move}
type state_p = {id: string; mutable hp: int; mutable atk: int; 
                mutable def: int; mutable spatk: int; mutable spdef: int;
                moves: state_m list; front: string; back: string; 
                desc: string; pok: pokemon}
type state_i = {id: string; mutable uses: int; value: float; itemtype: string}

type t = {battle: Battle.t; p1_party: state_p list; p2_party: state_p list; 
          turn: int;
          last_move_used: state_m option; p1_items: state_i list; 
          p2_items: state_i list;
          p1_active_id: string; p2_active_id: string; mutable missed: bool}

let convert_moves (mlist: Battle.move list) = 
  List.map (fun (x: Battle.move) -> {id = x.name; pp = 20; attack = x.attack; 
                                     move_type = x.movetype; mov = x}) mlist

let convert_party plist = 
  List.map (fun (x: Battle.pokemon) -> {id = x.name; hp = x.hp; atk = 0; 
                                        def = 0; spatk = 0; 
                                        spdef = 0; 
                                        moves = convert_moves x.moves;
                                        front = x.front; back = x.back; 
                                        desc = x.desc; pok = x}) plist

let convert_items (ilist: Battle.item list) =
  List.map (fun (x: Battle.item) -> 
      {id = x.name; uses = x.uses; 
       value = x.value; itemtype = x.itemtype}) ilist

let get_first_p (plist: state_p list) = 
  match plist with
  | h::t -> h.id
  | [] -> failwith "Empty party"

let init_state battle = 
  let p1_sp = convert_party (Battle.get_player_pokemon battle 1) in
  let p2_sp = convert_party (Battle.get_player_pokemon battle 2) in
  let p1_active = get_first_p p1_sp in
  let p2_active = get_first_p p2_sp in
  let p1_si = convert_items (Battle.get_items battle) in
  let p2_si = convert_items (Battle.get_items battle) in
  {battle = battle; p1_party = p1_sp; p2_party = p2_sp; turn = 1; 
   last_move_used = None;
   p1_items = p1_si; p2_items = p2_si; p1_active_id = p1_active; 
   p2_active_id = p2_active;
   missed = false}

type result = Legal of t | Illegal | Out

(** *)
type m = Megal of state_m | Immegal

let get_p state player =
  if player = 1 then List.find (fun (x: state_p) -> 
      x.id = state.p1_active_id) state.p1_party
  else List.find (fun (x: state_p) -> 
      x.id = state.p2_active_id) state.p2_party

let get_move state player (move:string list) = 
  let move = String.uppercase_ascii(String.concat " " move) in 
  let p = get_p state state.turn in
  match List.find_opt (fun (x: state_m) -> 
      String.uppercase_ascii(x.id) = move) p.moves with 
  | Some m -> Megal m
  | None -> Immegal

type s = Segal of state_p | Issegal

let get_swap_pokemon state player (name:string list) = 
  let pokemon_name = String.uppercase_ascii (String.concat " " name) in 
  if player = 1 
  then 
    match List.find_opt (fun (x:state_p) -> x.id = pokemon_name) 
            state.p1_party with
    | Some m -> if (m.hp > 0 && state.p1_active_id <> pokemon_name) then Segal m else Issegal
    | None -> Issegal
  else if player = 2 
  then 
    match List.find_opt (fun (x:state_p) -> x.id = pokemon_name) 
            state.p2_party with
    | Some m -> if (m.hp > 0 && state.p2_active_id <> pokemon_name) then Segal m else Issegal
    | None -> Issegal
  else Issegal

let effectiveness move_type target_type = 
  match move_type with
  | "Fire" -> (match target_type with 
      | "Fire" -> 0.5
      | "Water" -> 0.5
      | "Grass" -> 2.0
      | "Ice" -> 2.0
      | "Bug" -> 2.0
      | "Rock" -> 0.5
      | "Dragon" -> 0.5
      | "Steel" -> 2.0
      | _ -> 1.0)
  | "Water" -> (match target_type with 
      | "Fire" -> 2.0
      | "Water" -> 0.5
      | "Grass" -> 0.5
      | "Ground" -> 2.0
      | "Rock" -> 2.0
      | "Dragon" -> 0.5
      | _ -> 1.0)
  | "Grass" -> (match target_type with 
      | "Fire" -> 0.5
      | "Water" -> 2.0
      | "Grass" -> 0.5
      | "Poison" -> 0.5
      | "Ground" -> 2.0
      | "Flying" -> 0.5
      | "Bug" -> 0.5
      | "Rock" -> 2.0
      | "Dragon" -> 0.5
      | "Steel" -> 0.5
      | _ -> 1.0)
  | "Normal" -> (match target_type with 
      | "Rock" -> 0.5
      | "Ghost" -> 0.0
      | "Steel" -> 0.5
      | _ -> 1.0)
  | "Fighting" -> (match target_type with
      | "Normal" -> 2.0
      | "Ice" -> 2.0
      | "Poison" -> 0.5
      | "Flying" -> 0.5
      | "Psychic" -> 0.5
      | "Bug" -> 0.5
      | "Rock" -> 2.0
      | "Ghost" -> 0.0
      | "Dark" -> 2.0
      | "Steel" -> 2.0
      | _ -> 1.0)
  | "Electric" -> (match target_type with
      | "Water" -> 2.0
      | "Electric" -> 1.0
      | "Grass" -> 0.5
      | "Ground" -> 0.0
      | "Flying" -> 2.0
      | "Dragon" -> 0.5
      | _ -> 1.0)
  | "Ice" -> (match target_type with
      | "Water" -> 0.5
      | "Grass" -> 2.0
      | "Ice" -> 0.5
      | "Ground" -> 2.0 
      | "Flying" -> 2.0
      | "Dragon" -> 2.0
      | "Fire" -> 0.5
      | "Steel" -> 0.5
      |_ -> 1.0)
  | "Poison" -> (match target_type with
      | "Grass" -> 2.0
      | "Poison" -> 0.5
      | "Ground" -> 0.5 
      | "Bug" -> 1.0
      | "Rock" -> 0.5 
      | "Ghost" -> 0.5 
      | "Steel" -> 0.0
      |_ -> 1.0)
  | "Ground" -> (match target_type with
      | "Fire" -> 2.0
      | "Electric" -> 2.0
      | "Grass" -> 0.5 
      | "Poison" -> 2.0
      | "Flying" -> 0.0
      | "Bug" -> 0.5
      | "Rock" -> 2.0
      | "Steel" -> 2.0
      | _ -> 1.0)
  | "Flying" -> (match target_type with
      | "Electric" -> 0.5
      | "Grass" -> 2.0
      | "Fighting" -> 2.0
      | "Bug" -> 2.0
      | "Rock" -> 0.5
      | "Steel" -> 0.5
      | _ -> 1.0)
  | "Psychic" -> (match target_type with
      | "Fighting" -> 2.0
      | "Poison" -> 2.0
      | "Psychic" -> 0.5
      | "Dark" -> 0.0
      | "Steel" -> 0.5
      | _ -> 1.0)
  | "Bug" -> (match target_type with
      | "Fire" -> 0.5
      | "Grass" -> 2.0
      | "Fighting" -> 0.5
      | "Poison" -> 0.5
      | "Flying" -> 0.5
      | "Psychic" -> 2.0
      | "Ghost" -> 0.5
      | "Dark" -> 2.0
      | "Steel" -> 0.5
      | _ -> 1.0)
  | "Rock" -> (match target_type with
      | "Fire" -> 2.0
      | "Ice" -> 2.0
      | "Fighting" -> 0.5
      | "Ground" -> 0.5
      | "Flying" -> 2.0
      | "Bug" -> 2.0
      | "Steel" -> 0.5
      | _ -> 1.0)
  | "Ghost" -> (match target_type with
      | "Normal" -> 0.0
      | "Psychic" -> 2.0
      | "Ghost" -> 2.0
      | "Dark" -> 0.5
      | "Steel" -> 0.5
      | _ -> 1.0)
  | "Dragon" -> (match target_type with
      | "Dragon" -> 2.0
      | "Steel" -> 0.5
      | _ -> 1.0)
  | "Dark" -> (match target_type with
      | "Fighting" -> 0.5
      | "Psychic" -> 2.0
      | "Ghost" -> 2.0
      | "Dark"-> 0.5
      | "Steel" -> 0.5
      | _ -> 1.0)     
  | "Steel" -> (match target_type with
      | "Fire" -> 0.5
      | "Water" -> 0.5
      | "Electric" -> 0.5
      | "Ice" -> 2.0
      | "Rock" -> 2.0
      | "Steel" -> 0.5
      | _ -> 1.0)        
  | _ -> (match target_type with 
      | _ -> 1.0)

let stat_change_modifier stage =
  match stage with
  | -6 -> 0.25
  | -5 -> 0.29
  | -4 -> 0.33
  | -3 -> 0.4
  | -2 -> 0.5
  | -1 -> 0.66
  | 0 -> 1.0
  | 1 -> 1.5
  | 2 -> 2.0
  | 3 -> 2.5
  | 4 -> 3.0
  | 5 -> 3.5
  | 6 -> 4.0
  | _ -> 100000000000.0

(** [rng] is a random float between 0.85 and 1.0 for attack value variance
    as *)
let calculate_dmg state state_move state_user state_target =
  let stab = if state_user.desc = state_move.move_type then 1.5 else 1.0 in
  let random = (Random.float 0.15) +. 0.85 in
  let effect = effectiveness state_move.move_type state_target.desc in
  let modifier = stab *. random *. effect in
  let atkval = if state_move.mov.damage = "Physical" then 
      (stat_change_modifier (state_user.atk))*. float_of_int state_user.pok.atk 
    else (stat_change_modifier (state_user.spatk))*. 
         float_of_int state_user.pok.spatk in
  let defval = if state_move.mov.damage = "Physical" then 
      (stat_change_modifier (state_user.def))*. 
      float_of_int state_target.pok.def 
    else (stat_change_modifier (state_user.spdef))*. 
         float_of_int state_target.pok.spdef in
  let power = state_move.attack in
  let numerator = 3.0 *. ((float_of_int (power) *. (atkval)) /. (defval)) in 
  let miss_modifier = if Random.int 99 < state_move.mov.accuracy 
    then (state.missed <- false; 1) else (state.missed <- true; 0) in

  float_of_int(miss_modifier) *. ((numerator /. 50.0) +. 2.0) *. modifier


let get_other_p state player = 
  if player = 1 then List.find 
      (fun (x: state_p) -> x.id = state.p2_active_id) state.p2_party
  else List.find (fun (x: state_p) -> x.id = state.p1_active_id) state.p1_party

let attack state (move:string list) = 
  let p = get_p state state.turn in
  match get_move state state.turn move with
  | Megal m -> let other = get_other_p state state.turn in 
    if m.mov.damage = "None" then 
      if m.mov.target = "Self" then
        match m.mov.stat with
        | "atk" -> p.atk <- p.atk + m.attack; if p.atk < -6 
          then p.atk <- -6 else (); if p.atk > 6 then p.atk <- 6 else ();
        | "def" -> p.def <- p.def + m.attack; if p.def < -6 
          then p.def <- -6 else (); if p.def > 6 then p.def <- 6 else ();
        | "spatk" -> p.spatk <- p.spatk + m.attack; if p.spatk < -6 
          then p.spatk <- -6 else (); if p.spatk > 6 then p.spatk <- 6 else ();
        | "spdef" -> p.spdef <- p.spdef + m.attack; if p.spdef < -6 
          then p.spdef <- -6 else (); if p.spdef > 6 then p.spdef <- 6 else ();
        | _ -> failwith "Should not come here.";
      else
        match m.mov.stat with
        | "atk" -> other.atk <- other.atk + m.attack; 
          if other.atk < -6 then other.atk <- -6 else (); 
          if other.atk > 6 then other.atk <- 6 else ();
        | "def" -> other.def <- other.def + m.attack; 
          if other.def < -6 then other.def <- -6 else (); 
          if other.def > 6 then other.def <- 6 else ();
        | "spatk" -> other.spatk <- other.spatk + m.attack; 
          if other.spatk < -6 then other.spatk <- -6 else (); 
          if other.spatk > 6 then other.spatk <- 6 else ();
        | "spdef" -> other.spdef <- other.spdef + m.attack; 
          if other.spdef < -6 then other.spdef <- -6 else (); 
          if other.spdef > 6 then other.spdef <- 6 else ();
        | _ -> failwith "Should not come here.";
    else
      other.hp <- other.hp - int_of_float(calculate_dmg state m p other);
    Legal {state with  
           turn = if state.turn = 1 then 2 else 1; 
           last_move_used = Some m;
          }
  | Immegal -> Illegal

let swap state (name:string list) = 
  match get_swap_pokemon state state.turn name with
  | Segal m ->  let pokemon_name = 
                  String.uppercase_ascii(String.concat " " name) in
    let p = get_p state state.turn in
    p.atk <- 0; p.def <- 0; p.spatk <- 0; p.spdef <- 0;
    if state.turn = 1 then Legal {state with p1_active_id = pokemon_name; 
                                             turn = 2; last_move_used = None} 
    else Legal {state with p2_active_id = pokemon_name; turn = 1; 
                           last_move_used = None}
  | Issegal -> Illegal

let use_item state item = 
  match item.itemtype with 
  | "Healing" -> let p = get_p state state.turn in 
    (if state.turn = 1 then
       (match (Battle.getpokemonfromname state.battle.player1_pokemon p.id) with
        | Some bp -> if p.hp + int_of_float(item.value) > bp.hp 
          then p.hp <- bp.hp else p.hp <- p.hp + int_of_float(item.value)
        | None -> failwith "Should not come here")
     else
       (match (Battle.getpokemonfromname state.battle.player2_pokemon p.id) with
        | Some bp -> if p.hp + int_of_float(item.value) > bp.hp 
          then p.hp <- bp.hp else p.hp <- p.hp + int_of_float(item.value)
        | None -> failwith "Should not come here"));
    if state.turn = 1 then
      {state with turn = 2; last_move_used = None}
    else
      {state with turn = 1; last_move_used = None}
  | _ -> failwith "Invalid item type"

let use state (item: string list) = 
  let item = String.uppercase_ascii (String.concat " " item) in 
  if state.turn = 1 then 
    let i_opt = List.find_opt 
        (fun x -> item = String.uppercase_ascii(x.id)) state.p1_items in
    match i_opt with
    | Some i -> if i.uses > 0 then 
        (i.uses <- i.uses - 1; Legal (use_item state i)) else 
        Out
    | None -> Illegal
  else 
    let i_opt = List.find_opt 
        (fun x -> item = String.uppercase_ascii(x.id)) state.p2_items in
    match i_opt with
    | Some i -> if i.uses>0 then

        (i.uses <- i.uses - 1;
         Legal (use_item state i)) else Out

    | None -> Illegal

let get_last_move_used state = 
  match state.last_move_used with 
  | Some m -> m.id 
  | None -> ""

let get_turn state =
  state.turn

let rec get_statep_from_name name (lst: state_p list) = 
  List.find (fun (x: state_p) -> x.id = name) lst


let get_hp state player =
  let p = get_p state player in p.hp

let get_active_pokemon state player =
  match player with
  | 1 -> state.p1_active_id
  | 2 -> state.p2_active_id
  | i -> failwith "player error" 