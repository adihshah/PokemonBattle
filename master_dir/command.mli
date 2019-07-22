(**
   Parsing of player commands.
*)


(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["fight Water Gun"], then the object phrase is 
      [["Water"; "Gun"]].
    - If the player command is ["fight Water     Gun"], then the object phrase 
      is again [["Water"; "Gun"]]. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Fight of object_phrase
  | Use of object_phrase
  | Swap of object_phrase
  | Run

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    fight   Water Gun"] is [Fight ["Water"; "Gun"]]
    - [parse "run"] is [Run]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "run" nor "fight",
    or if the verb is "run" and there is a non-empty object phrase,
    or if the verb is "fight" and there is an empty object phrase.*)
val parse : string -> command


(** [parsehelper lst] returns a list [m] that satisfies these conditions: 
    - contains all elements of lst except for the head
    - if lst is empty returns lst
    - if lst contains 1 element returns [] *)
val parsehelper : 'a list -> 'a list

(** [removespace lst acc] returns a list [m] that satisfies these conditions: 
    - contains all elements of lst that are not the empty string "" *)
val removespace:string list -> string list -> string list