open Yojson.Basic.Util

type name = string
type desc = string
type hp = int
type move = {name: string; attack: int; movetype:string; accuracy:int; 
             damage:string; target:string; stat:string; pp:int}
type moves = move list
type pokemon = {name:name; desc:desc; hp:hp; atk: int; spatk: int; def: int; 
                spdef: int; moves:moves; front:string; back: string}
type item = {name: string; itemtype: string; value: float; uses: int}

type t = {player1_pokemon: pokemon list; player2_pokemon: pokemon list;
          items : item list}

let name_of_json json = 
  json |> member "name" |> to_string

let atk_of_json json = 
  json |> member "atk" |> to_int

let sp_atk_of_json json = 
  json |> member "spatk" |> to_int

let def_of_json json = 
  json |> member "def" |> to_int

let sp_def_of_json json = 
  json |> member "spdef" |> to_int

let desc_of_json json = 
  json |> member "desc" |> to_string

let hp_of_json json =
  json |> member "hp" |> to_int

let move_of_json json = {
  name = json |> member "name" |> to_string;
  attack = json |> member "attack" |> to_int;
  movetype = json |> member "type" |> to_string;
  accuracy = json |> member "accuracy" |> to_int;
  damage = json |> member "damage" |> to_string;
  target = json |> member "target" |> to_string;
  stat= json |> member "stat" |> to_string;
  pp = json |> member "pp" |> to_int;

}

let moves_of_json json =  json |> member "moves" |> to_list |> 
                          List.map move_of_json

let front_of_json json = json |> member "front" |> to_string

let back_of_json json = json |> member "back" |> to_string

let pokemon_of_json json = {
  name = name_of_json json;
  desc = desc_of_json json;
  hp = hp_of_json json;
  moves = moves_of_json json;
  front = front_of_json json;
  back = back_of_json json;
  atk = atk_of_json json;
  def = def_of_json json;
  spatk = sp_atk_of_json json;
  spdef = sp_def_of_json json;
}

let pokemonlist_of_json json = 
  json |> member "list_of_pokemon"|> to_list |> 
  List.map pokemon_of_json


let init_battle p1 p2 items = 
  {player1_pokemon = p1; player2_pokemon = p2; items = items}

let get_name (pokemon: pokemon) = 
  pokemon.name 

let get_desc pokemon =
  pokemon.desc

let get_hp pokemon =
  pokemon.hp

let get_moves pokemon =
  pokemon.moves

let get_player_hp battle player =
  if player = 1 then get_hp (List.hd battle.player1_pokemon) 
  else get_hp (List.hd battle.player2_pokemon)

let get_player_pokemon battle player =
  if player = 1 then battle.player1_pokemon else battle.player2_pokemon

let get_other_player_pokemon battle player =
  if player = 1 then battle.player2_pokemon else battle.player1_pokemon

let get_items battle = 
  battle.items

let get_front pokemon =
  pokemon.front

let get_back pokemon = 
  pokemon.back

let get_items battle =
  battle.items

let getpokemonfromname (lst: pokemon list) name =
  List.find_opt (fun (x: pokemon) -> x.name = name) lst

let get_attack_val move =
  move.attack

let get_move_type move = 
  move.movetype

let get_move_name (move: move) =
  move.name

let itemtype_of_json json = json |> member "itemtype" |> to_string

let value_of_json json = json |> member "value" |> to_float

let uses_of_json json = json |> member "uses" |> to_int


let item_of_json json = {
  name = name_of_json json;
  itemtype = itemtype_of_json json;
  value = value_of_json json;
  uses = uses_of_json json;
}


let itemlist_of_json json = 
  json |> member "list_of_items"|> to_list |> 
  List.map item_of_json