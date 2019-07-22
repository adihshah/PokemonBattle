(** The type of a name *)
type name = string

(** The type of a pokemon type *)
type desc = string

(** The type of hp *)
type hp = int

(** A move has a name and an attack value *)
type move = {name: string; attack: int; movetype: string; accuracy:int; 
             damage:string; target:string; stat:string; pp:int}

(** The type of a pokemons moves *)
type moves = move list

(** The type of an item *)
type item = {name: string; itemtype: string; value: float; uses: int}

(** A pokemon has a name, desc, hp, moves, stats, and front art and back art *)
type pokemon = {name:name; desc:desc; hp:hp; atk: int; spatk: int; def: int; 
                spdef: int; moves:moves; front: string; back:string}

(** The type of a battle *)
type t = {player1_pokemon: pokemon list; player2_pokemon: pokemon list;
          items: item list;}


(** [name_of_json json] Extracts a name from a json *)
val name_of_json: Yojson.Basic.json -> string

(** [desc_of_json json] Extracts a desc from a json *)
val desc_of_json: Yojson.Basic.json -> string

(** [hp_of_json json] Extracts an hp from a json *)
val hp_of_json : Yojson.Basic.json -> int

(** [move_of_json json] Extracts a move from a json *)
val move_of_json: Yojson.Basic.json -> move

(** [moves_of_json json] Extracts a list of moves from a json *)
val moves_of_json:Yojson.Basic.json -> moves

(** [pokemon_of_json json] Extracts a pokemon from a json *)
val pokemon_of_json: Yojson.Basic.json -> pokemon

(** [pokemonlist_of_json json] Extracts a list of pokemon from a json *)
val pokemonlist_of_json: Yojson.Basic.json -> pokemon list

(** [item_of_json json] Extracts an item from a json *)
val item_of_json: Yojson.Basic.json -> item

(** [itemlist_of_json json] Extracts a list of items from a json *)
val itemlist_of_json: Yojson.Basic.json -> item list

(** [get_name pokemon] Is a given pokemon's name *)
val get_name: pokemon -> name

(** [get_desc pokemon] Is a given pokemons type *)
val get_desc: pokemon -> name

(** [get_moves pokemon] Is a list of a given pokemons moves *)
val get_moves: pokemon -> move list

(** [get_player_pokemon state num] Is the pokemon of player 1 or 2 
    (denoted by num) *)
val get_player_pokemon: t -> int -> pokemon list

(** [get_other_player_pokemon state num] Is the pokemon of the opposite 
    player given denoted by num in the current state *)
val get_other_player_pokemon: t -> int -> pokemon list

(** [get_player_hp state num] Is the maximum hp of player 1 or 2's pokemon
    denoted by num *)
val get_player_hp: t -> int -> int

(** [get_front pokemon] Is the front ASCII art of a given pokemon *)
val get_front: pokemon -> string

(** [get_front pokemon] Is the back ASCII art of a given pokemon *)
val get_back: pokemon -> string

(** [getpokemonfromname lst name] Is a data type pokemon from its name *)
val getpokemonfromname: pokemon list -> string -> pokemon option

(** [get_attack_val move] Is the attack value of a given move *)
val get_attack_val: move -> int

(** [get_attack_val move] Is the attack value of a given move *)
val get_move_type: move -> string

(** [get_move_name move] is the [name] field of [move]. *)
val get_move_name: move -> string

(** [init_battle pokemon1 pokemon2] Given two pokemon [pokemon1] and [pokemon2],
    is a new battle *)
val init_battle: pokemon list -> pokemon list -> item list -> t

(** [get_items battle] is the item list available in the given [battle] *)
val get_items: t -> item list