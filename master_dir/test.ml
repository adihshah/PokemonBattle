open OUnit2
open Battle
open Command
open State

let make_command_tests  
    (name : string) 
    (input: string) 
    (expected_output: command) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (parse input))

let command_tests =
  [
    (* TODO: add tests for the Command module here *)
    make_command_tests "testquit1" "run" Run;
    make_command_tests "testquit2" "         run    " Run;

    make_command_tests "testlegal1" "fight Water Gun" (Fight["Water";"Gun"]);

    make_command_tests "testlegal2" 
      "fight Body Slam" (Fight["Body";"Slam"]);

    make_command_tests "testuse1" 
      "use Potion" (Use["Potion"]);

    make_command_tests "testuse2" 
      "use berry" (Use["berry"]);

    make_command_tests "testswap1" 
      "swap Abra" (Swap["Abra"]);

    make_command_tests "testswap2" 
      "swap charmander" (Swap["charmander"]);

    "testempty1" >:: (fun _ -> assert_raises(Empty) (fun () -> parse ("") ));

    "testempty2" >:: (fun _ -> assert_raises(Empty)
                         (fun () -> parse ("      ")));

    "testmalformed1" >:: (fun _ -> assert_raises(Malformed) 
                             (fun () -> parse ("run Water Gun      ") ));
    "testmalformed2" >:: (fun _ -> assert_raises(Malformed) 
                             (fun () -> parse ("fight") ));

    "testmalformed3" >:: (fun _ -> assert_raises(Malformed) 
                             (fun () -> parse ("go       ") ));

  ]

let watergunmove = {
  name = "Water Gun";
  attack = 40;
  movetype = "Water";
  accuracy = 100;
  damage = "Special";
  target = "Opponent";
  stat= "None";
  pp = 25
}

let watergun = {id = "Water Gun"; pp =  20; attack = 50 ; move_type =  "Normal"; mov = watergunmove}


let squirtlepok = {
  name = "SQUIRTLE";
  desc = "Water"; 
  hp = 44; 
  atk = 48; 
  spatk =  50;
  def = 65; 
  spdef = 64;
  moves = [watergunmove]; 
  front = "";
  back = "";
}

let squirtle = {id = "SQUIRTLE"; hp = 44; atk = 48;def = 65; spatk = 50;spdef = 64; 
                moves = [watergun]; front = ""; back = ""; desc = "Water"; pok = squirtlepok; }

let charmanderpok = {
  name = "CHARMANDER";
  desc = "Fire"; 
  hp = 39; 
  atk = 52; 
  spatk =  43;
  def = 60; 
  spdef = 50;
  moves = []; 
  front = "";
  back = "";
}

let charmander = {id = "CHARMANDER"; hp = 39; atk = 52;def = 60; spatk = 43;spdef = 50; 
                  moves = []; front = ""; back = ""; desc = "Fire"; pok = charmanderpok; }

let bulbapok = {
  name = "BULBASAUR";
  desc = "Fire"; 
  hp = 45; 
  atk = 49; 
  spatk =  49;
  def = 49; 
  spdef = 65;
  moves = []; 
  front = "";
  back = "";
}

let bulba = {id = "BULBASAUR"; hp = 45; atk = 49;def = 49; spatk = 49;spdef = 65; 
             moves = []; front = ""; back = ""; desc = "Grass"; pok = bulbapok; }


let potion = {id = "Potion"; uses = 2; value = 20.0; itemtype = "Healing"} 
let potionitem = {name = "Potion"; itemtype = "Healing"; value = 20.0; uses = 2}
let oran_berry = {id = "Oran Berry"; uses = 1; value = 10.0; itemtype = "Healing"}
let oran_berryitem = {name = "Oran Berry"; itemtype = "Healing"; value = 10.0; uses = 1} 
let potion_oneless_use = {id = "Potion"; uses = 1; value = 20.0; itemtype = "Healing"} 
let potion_twoless_use = {id = "Potion"; uses = 0; value = 20.0; itemtype = "Healing"} 
let oran_berry_oneless_use = {id = "Oran Berry"; uses = 0; value = 10.0; itemtype = "Healing"}

let init_battle p1 p2 items = 
  {player1_pokemon = p1; player2_pokemon = p2; items = items}

let battle = init_battle ([charmanderpok; squirtlepok]) ([squirtlepok]) ([potionitem;oran_berryitem])


let first_swap_state = {battle = battle; p1_party = [charmander;squirtle]; p2_party = [bulba]; 
                        turn = 1;
                        last_move_used = Some watergun; p1_items = [potion;oran_berry]; 
                        p2_items = [potion;oran_berry];
                        p1_active_id = "SQUIRTLE"; p2_active_id = "BULBASAUR"; missed = false}

let second_swap_state = {battle = battle; p1_party = [charmander;squirtle]; p2_party = [bulba]; 
                         turn = 2;
                         last_move_used = None; p1_items = [potion;oran_berry]; 
                         p2_items = [potion;oran_berry];
                         p1_active_id = "CHARMANDER"; p2_active_id = "BULBASAUR"; missed = false}


let make_swap_test
    (name : string) 
    (state: State.t) 
    (pokename: string list)
    (expected_output: result) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (swap state pokename))

let swap_tests = [
  make_swap_test "swap squirtle with charmander"  first_swap_state ["charmander"] (Legal (second_swap_state));
  make_swap_test "swap squirtle with squirtle"  first_swap_state ["squirtle"] Illegal;
  make_swap_test "swap charmander with charmander"  second_swap_state ["charmander"] Illegal;

  make_swap_test "swap squirtle with empty"  first_swap_state [""] Illegal;
  make_swap_test "swap charmander with empty"  second_swap_state [""] Illegal;
]

let first_item_state = {battle = battle; p1_party = [charmander;squirtle]; p2_party = [squirtle]; 
                        turn = 1;
                        last_move_used = Some watergun; p1_items = [potion;oran_berry]; 
                        p2_items = [potion;oran_berry];
                        p1_active_id = "CHARMANDER"; p2_active_id = "SQUIRTLE"; missed = false}

let second_item_state = {battle = battle; p1_party = [charmander;squirtle]; p2_party = [squirtle]; 
                         turn = 2;
                         last_move_used = None; p1_items = [potion_oneless_use;oran_berry]; 
                         p2_items = [potion;oran_berry];
                         p1_active_id = "CHARMANDER"; p2_active_id = "SQUIRTLE"; missed = false}

let third_item_state = {battle = battle; p1_party = [charmander;squirtle]; p2_party = [squirtle]; 
                        turn = 1;
                        last_move_used = None; p1_items = [potion_oneless_use;oran_berry]; 
                        p2_items = [potion_oneless_use;oran_berry];
                        p1_active_id = "CHARMANDER"; p2_active_id = "SQUIRTLE"; missed = false}

let fourth_item_state = {battle = battle; p1_party = [charmander;squirtle]; p2_party = [squirtle]; 
                         turn = 2;
                         last_move_used = None; p1_items = [potion_twoless_use;oran_berry]; 
                         p2_items = [potion_oneless_use;oran_berry];
                         p1_active_id = "CHARMANDER"; p2_active_id = "SQUIRTLE"; missed = false}

let fifth_item_state = {battle = battle; p1_party = [charmander;squirtle]; p2_party = [squirtle]; 
                        turn = 1;
                        last_move_used = None; p1_items = [potion_twoless_use;oran_berry]; 
                        p2_items = [potion_oneless_use;oran_berry_oneless_use];
                        p1_active_id = "CHARMANDER"; p2_active_id = "SQUIRTLE"; missed = false}

let sixth_item_state = {battle = battle; p1_party = [charmander;squirtle]; p2_party = [squirtle]; 
                        turn = 2;
                        last_move_used = None; p1_items = [potion_twoless_use;oran_berry_oneless_use]; 
                        p2_items = [potion_oneless_use;oran_berry_oneless_use];
                        p1_active_id = "CHARMANDER"; p2_active_id = "SQUIRTLE"; missed = false}

let make_use_test
    (name : string)
    (state: State.t)
    (item: string list)
    (expected_output : State.result) : test =
  name >:: (fun _ ->
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (use state item))

let use_tests = [
  make_use_test "use test 1" first_item_state ["Potion"] (Legal second_item_state);
  make_use_test "use test 2" second_item_state ["Potion"] (Legal third_item_state);
  make_use_test "use test 3" third_item_state ["Potion"] (Legal fourth_item_state);
  make_use_test "use test 4" fourth_item_state ["Oran Berry"] (Legal fifth_item_state);

]

let tests =
  "test suite for A6"  >::: List.flatten [
    command_tests;
    swap_tests;
    use_tests;
  ]

let _ = run_test_tt_main tests