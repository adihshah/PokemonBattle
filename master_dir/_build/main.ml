open Battle
open State
open Command

(* [print_battle_desc battle state] Prints the following details of the battle 
   in the current state
   - ASCII representation of both pokemon
   - Type of the opponent Pokemon
   - Player whose turn it is
   - Both players' pokemon's HP
   - Options the player has in battle
   - Moves available to the player
   - Prompts user for a command *)
let print_battle_desc battle state = 
  let player = State.get_turn state in
  let other_player = if player = 1 then 2 else 1 in
  let player_hp = State.get_hp state player in 
  let other_player_hp = State.get_hp state other_player in
  let player_pokemon = if state.turn = 1 then get_statep_from_name 
        state.p1_active_id state.p1_party else get_statep_from_name 
        state.p2_active_id state.p2_party in
  let other_player_pokemon = if state.turn = 1 then get_statep_from_name 
        state.p2_active_id state.p2_party else get_statep_from_name 
        state.p1_active_id state.p1_party in
  let pokemon_name = if state.turn = 1 then state.p1_active_id else 
      state.p2_active_id in
  let other_pokemon_name = if state.turn = 1 then state.p2_active_id else 
      state.p1_active_id in
  let other_pokemon_desc = other_player_pokemon.desc in 
  let pokemon_moves = player_pokemon.moves in 
  let close_pokemon = player_pokemon.back in 
  let far_pokemon = other_player_pokemon.front in
  let type_of_pokemon = player_pokemon.desc in
  let type_of_other_pokemon = other_player_pokemon.desc in



  let color_from_type (typ:string) :ANSITerminal.style =
    match typ with
    | "Water" -> Foreground Blue
    | "Fire" -> Foreground Red
    | "Fighting" -> Foreground White
    | "Grass" -> Foreground Green
    | "Bug" -> Foreground Green
    | "Electric" -> Foreground Yellow
    | "Psychic" -> Foreground Magenta
    | "Ghost" -> Foreground Magenta
    | "Poison" -> Foreground Magenta
    | "Ice" -> Foreground Cyan
    | _ -> Foreground White
  in
  let color_from_hp (hp:int) :(ANSITerminal.style) =
    if hp <= 0 then 
      Foreground Black
    else if hp < int_of_float(0.25*.float_of_int(player_pokemon.hp)) then 
      Foreground Red
    else if hp < int_of_float(0.66*.float_of_int(player_pokemon.hp)) then 
      Foreground Yellow
    else Foreground Green in
  let rec moveprint (moves:state_m list) = match moves with
    | [] -> ()
    | h::t ->  
      ANSITerminal.(print_string [color_from_type (h.move_type)] (h.id));
      print_string(" | ");
      moveprint t
  in 
  let rec pokeprint (party:state_p list) = match party with
    | [] -> ()
    | h::t ->  
      ANSITerminal.(print_string [color_from_hp (h.hp)] (h.id));
      print_string(" | ");
      pokeprint t
  in 
  let rec itemprint (bag:state_i list) = match bag with
    | [] -> ()
    | h::t ->  
      ANSITerminal.(print_string [white] (h.id^": "^string_of_int(h.uses)));
      print_string(" | ");
      itemprint t
  in 
  print_newline ();

  ANSITerminal.(print_string [color_from_type type_of_other_pokemon] 
                  far_pokemon);
  print_newline ();

  ANSITerminal.(print_string [color_from_type type_of_pokemon] close_pokemon);
  print_newline ();
  (match state.last_move_used with
   | Some m -> 
     let move_type = m.move_type in
     let effectiveness = State.effectiveness move_type type_of_pokemon in
     let move_dmg = m.mov.damage in
     let move_stat = m.mov.stat in
     let move_degree = m.mov.attack in
     let move_target = m.mov.target in
     let print_last_action = if move_type <> "" 
       then ANSITerminal.(print_string [white] 
                            ("\n"^other_pokemon_name^" used "^
                             (get_last_move_used state)^"!")) in
     let print_stat_desc = begin match move_dmg with 
       | "None" -> (match move_target with
           | "Self" -> (match move_degree with
               | -1 -> (match move_stat with
                   | "atk" -> ANSITerminal.(print_string [white] 
                                              ("\n"^other_pokemon_name^"'s 
                                              Attack fell!"));
                   | "def" -> ANSITerminal.(print_string [white] 
                                              ("\n"^other_pokemon_name^"'s 
                                              Defense fell!"));
                   | "spatk" -> ANSITerminal.(print_string [white] 
                                                ("\n"^other_pokemon_name^"'s 
                                                Special Attack fell!"));
                   | "spdef" -> ANSITerminal.(print_string [white] 
                                                ("\n"^other_pokemon_name^"'s 
                                                Special Defense fell!"));
                   | _ -> ANSITerminal.(print_string [white] (""));)
               | 1 -> (match move_stat with
                   | "atk" -> ANSITerminal.(print_string [white] 
                                              ("\n"^other_pokemon_name^"'s 
                                              Attack rose!"));
                   | "def" -> ANSITerminal.(print_string [white] 
                                              ("\n"^other_pokemon_name^"'s 
                                              Defense rose!"));
                   | "spatk" -> ANSITerminal.(print_string [white] 
                                                ("\n"^other_pokemon_name^"'s 
                                                Special Attack rose!"));
                   | "spdef" -> ANSITerminal.(print_string [white] 
                                                ("\n"^other_pokemon_name^"'s 
                                                Special Defense rose!"));
                   | _ -> ANSITerminal.(print_string [white] (""));)
               | _ -> ANSITerminal.(print_string [white] "");)
           | "Opponent" -> (match move_degree with
               | -1 -> (match move_stat with
                   | "atk" -> ANSITerminal.(print_string [white] 
                                              ("\n"^pokemon_name^"'s 
                                              Attack fell!"));
                   | "def" -> ANSITerminal.(print_string [white] 
                                              ("\n"^pokemon_name^"'s 
                                              Defense fell!"));
                   | "spatk" -> ANSITerminal.(print_string [white] 
                                                ("\n"^pokemon_name^"'s 
                                                Special Attack fell!"));
                   | "spdef" -> ANSITerminal.(print_string [white] 
                                                ("\n"^pokemon_name^"'s 
                                                Special Defense fell!"));
                   | _ -> ANSITerminal.(print_string [white] (""));)
               | 1 -> (match move_stat with
                   | "atk" -> ANSITerminal.(print_string [white] 
                                              ("\n"^pokemon_name^"'s 
                                              Attack rose!"));
                   | "def" -> ANSITerminal.(print_string [white] 
                                              ("\n"^pokemon_name^"'s 
                                              Defense rose!"));
                   | "spatk" -> ANSITerminal.(print_string [white] 
                                                ("\n"^pokemon_name^"'s 
                                                Special Attack rose!"));
                   | "spdef" -> ANSITerminal.(print_string [white] 
                                                ("\n"^pokemon_name^"'s 
                                                Special Defense rose!"));
                   | _ -> ANSITerminal.(print_string [white] (""));)
               | _ -> ANSITerminal.(print_string [white] "");)
           | _ -> ANSITerminal.(print_string [white] "");)
       | _ -> ANSITerminal.(print_string [white] ""); end in
     let print_missed_desc =
       if state.missed = false then print_string("")  else 
         ANSITerminal.(print_string [white] "\nBut it missed!") in

     let print_effect_desc = begin match effectiveness with 
       | 0.5 -> (if not state.missed then 
                   ANSITerminal.(print_string [white] 
                                   "\nIt's not very effective...") 
                 else print_string "";)
       | 2.0 -> (if not state.missed 
                 then ANSITerminal.(print_string [white] 
                                      "\nIt's super effective!") 
                 else print_string "";)
       | 0.0 -> (if not state.missed 
                 then ANSITerminal.(print_string [white] 
                                      ("\nIt doesn't affect "^
                                       pokemon_name^"...")) 
                 else print_string "";)
       | _ -> ANSITerminal.(print_string [white] "\n"); end in
     print_newline ();
     print_last_action;
     print_stat_desc;
     print_missed_desc;
     print_effect_desc;


   | None -> print_newline (););



  ANSITerminal.(print_string [green] ("\nIt is player "^(string_of_int player)^
                                      "'s turn. "));
  ANSITerminal.(print_string [red] ("\n"^other_pokemon_name^" is a "^
                                    other_pokemon_desc^" Pokemon."));
  ANSITerminal.(print_string [green] ("\nPlayer "^(string_of_int other_player)^
                                      "'s HP: "^
                                      (string_of_int other_player_hp)));
  ANSITerminal.(print_string [green] ("\nPlayer "^(string_of_int player)^
                                      "'s HP: "^(string_of_int player_hp)));


  ANSITerminal.(print_string [white] ("\nYou can FIGHT [insert move], SWAP 
  [insert pokemon], USE [insert item] or RUN."));
  ANSITerminal.(print_string [yellow] ("\nYour Pokemon: "));
  (match player with
   | 1 -> pokeprint state.p1_party; 
   | 2 -> pokeprint state.p2_party; 
   | _ -> pokeprint state.p1_party; );
  ANSITerminal.(print_string [yellow] ("\nYour Items: "));
  (match player with
   | 1 -> itemprint state.p1_items; 
   | 2 -> itemprint state.p2_items; 
   | _ -> itemprint state.p1_items; );
  ANSITerminal.(print_string [white] ("\n"^pokemon_name^"'s moves: "));



  moveprint pokemon_moves;
  print_newline ();
  ANSITerminal.(print_string [white] ("\nWhat will "^pokemon_name^" do?"))

(** [handle_malformed input state] returns the argument [state]. If parsing its
    value using Command.parse raises exceptions Empty or Malformed then it
    performs the folowing actions:
    - prints an error message *)
let handle_malformed (input:string) state : State.t = 
  try ((let _ = Command.parse input in state))
  with 
  |Command.Empty -> print_string "Please enter a valid input"; state
  |Command.Malformed -> print_string "Please enter a valid input"; state

let rec all_fainted lst = 
  match lst with 
    h::t -> if h.hp > 0 then false else all_fainted t
  |[] -> true

let is_battle_over state = 
  let p1list = state.p1_party in
  let p2list = state.p2_party in 
  all_fainted p1list || all_fainted p2list

(** [is_battle_over state] 
    Returns true if a player has reached 0 or less hp. False otherwise *)
(** let is_battle_over state = 
    let hp1 = get_hp state 1 in 
    let hp2 = get_hp state 2 in 
    hp1 <= 0 || hp2 <= 0*)

(** [winner state] Returns the winner of the game *)
(** let winner state =
    let hp1 = get_hp state 1 in 
    if hp1<=0 then 2 else 1*)
let winner state = 
  let p1list = state.p1_party in
  if all_fainted p1list then 2 else 1



(** [play_game_helper battle state status] does the following actions:
    - prints the description of the battle's current status
    - prompts the reader for an input
    - calls Command.parse on the input
    - if this call raises an exception Empty or Malformed, then prints an error
      message and prompts for another input, and calls itself with the same 
      arguments 
    - if this call is of type Run, then ends game
    - if this call is of type Fight of l, then this function calls itself with 
      the same argument for battle but a different argument for state as follows
      :
      The state is the result of
      State.attack (state) (l)
*)let rec play_game_helper battle state status = 

    let handleswap state : unit= 
      let input = read_line() in

      try (let x = Command.parse input in 
           match x with 
           | Run -> (print_string "You fled from battle. Game Over."; 
                     print_newline ();
                     exit 0)
           | Swap s -> begin 
               match State.swap state s with 
                 Illegal ->
                 print_string 
                   "This pokemon does not exist. Input another value."; 
                 print_newline (); 
                 play_game_helper battle state 0 
               |Legal (st) -> play_game_helper battle st 1
               | Out -> print_string 
                          "You can't swap this pokemon in. It has 0 HP. 
                          Please enter another command"; 
                 print_newline (); 
                 play_game_helper battle state 0
             end
           | _ -> print_string 
                    "This action is not available. Input another value"; 
             print_newline (); 
             play_game_helper battle state 0 )


      with 
      | Command.Empty -> 
        print_string "Please enter a valid input"; print_newline ();
        play_game_helper battle state 0
      | Command.Malformed -> 
        print_string "Please enter a valid input";
        print_newline (); 
        play_game_helper battle state 0  in 

    if is_battle_over state 
    then
      (print_string ("");
       (ANSITerminal.(print_string [cyan] ("Player "^string_of_int(winner state)
                                           ^" wins!")));
       print_newline ();
       exit 0)
    else 


    if status = 1 then
      if get_hp state state.turn <= 0 then 
        ((
          ANSITerminal.(print_string [green] 
                          ("\nIt is player "^
                           (string_of_int (State.get_turn state))^
                           "'s turn. "));
          ANSITerminal.(print_string [red] 
                          ("\nYour Pokemon fainted. 
                          Please choose another pokemon to swap with"));
          print_newline ();
          handleswap state);)

      else

        ((print_battle_desc (battle) (state);
          print_newline ());)

    else 
    if get_hp state state.turn <= 0 then 
      ((ANSITerminal.(print_string [green] 
                        ("\nIt is player "^(string_of_int 
                                              (State.get_turn state))^
                         "'s turn. "));
        ANSITerminal.(print_string [red] ("\nYour Pokemon fainted. 
        Please choose another pokemon to swap with"));
        print_newline ();
        handleswap state);)
    else 
      print_string "Enter your command here: ";

    let input = read_line() in

    try (let x = Command.parse input in 
         match x with 
         | Run -> (print_string "You fled from battle. Game Over."; 
                   print_newline ();
                   exit 0)
         | Use u -> begin 
             match State.use state u with 
               Illegal ->
               print_string 
                 "This item does not exist. Input another value."; 
               print_newline (); 
               play_game_helper battle state 0 
             |Legal (st) -> play_game_helper battle st 1
             | Out -> print_string 
                        "You are out of uses for this item."; 
               print_newline (); 
               play_game_helper battle state 0
           end
         | Swap s -> begin 
             match State.swap state s with 
               Illegal ->
               print_string 
                 "This pokemon does not exist. Input another value."; 
               print_newline (); 
               play_game_helper battle state 0 
             |Legal (st) -> play_game_helper battle st 1
             | Out -> print_string 
                        "You can't swap this pokemon in. It has 0 HP. 
                        Please enter another command"; 
               print_newline (); 
               play_game_helper battle state 0
           end
         | Fight m -> match State.attack (state) (m) with
             Illegal -> 
             print_string 
               "This attack is not accessible. Input another value"; 
             print_newline (); 
             play_game_helper battle state 0 
           |Legal(st)-> play_game_helper battle st 1
           |Out -> play_game_helper battle state 1 )


    with 
    | Command.Empty -> 
      print_string "Please enter a valid input"; print_newline ();
      play_game_helper battle state 0
    | Command.Malformed -> 
      print_string "Please enter a valid input";
      print_newline (); 
      play_game_helper battle state 0

(** [remove lst el out] Removes an element from a list of elements with names 
    nd returns the output as list out *)
let rec remove_helper lst el out = 
  match lst with 
    h::t -> if h.name = el then remove_helper t el out else remove_helper t el
        (h::out)
  | [] -> out    

(** [remove lst el] Removes an element from a list of elements with names *)
let remove lst el = 
  remove_helper lst el []

(** [choose lst num status] Prompts the player with number[num] to choose from a 
    list of pokemon, then removes the chosen pokemon from the list of 
    available pokemon. If status is 0, then prints a string asking the player to
    choose their pokemon, else asks the player to enter a valid pokemon name *)
let rec choose lst num status = 
  if status = 0 then
    print_string("Player " ^ string_of_int(num) ^ ": Choose your Pokemon: ")
  else 
    print_string("Please enter a valid Pokemon: ");
  let name = read_line () in 

  match Battle.getpokemonfromname lst (String.uppercase_ascii name) with
  | Some p -> (p, remove lst p.name)
  | None -> choose lst num 1

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =

  let game = Yojson.Basic.from_file f in 
  let lst = Battle.pokemonlist_of_json game in
  let items = itemlist_of_json game in 

  let rec pokeprint (pokelist:pokemon list) = match pokelist with
    | [] -> ()
    | h::t ->  
      ANSITerminal.(print_string [yellow] h.name);
      print_string(" | ");
      pokeprint t
  in 

  ANSITerminal.(print_string [yellow]
                  "Starting Pokemon: ");
  pokeprint lst;
  print_newline();


  let p1 = choose lst 1 0 in
  let p2 = choose (snd(p1)) 2 0 in 
  let p3 =  choose (snd(p2)) 1 0 in
  let p4 = choose (snd(p3)) 2 0 in
  let p5 =  choose (snd(p4)) 1 0 in
  let p6 =  choose (snd(p5)) 2 0 in
  let battle = Battle.init_battle ([fst(p1);fst(p3); fst(p5)]) 
      ([fst(p2);fst(p4); fst(p6)]) items in
  let state = State.init_state battle in

  play_game_helper battle state 1


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Pokemon Battle.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(** Execute the game engine. *)
let () = main ()

