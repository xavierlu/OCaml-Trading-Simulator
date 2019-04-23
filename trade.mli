open Scraper

(** [state] is the representation of the current state of the simulator.
    Keeps track of the user's capital to buy more stocks, what stocks they
    currently own, the value of their account, the current day, and a list of 
    all possible future dates*)
type state = {
  balance: float;
  portfolio: (string * int) list;
  short_positions: (string * string * int) list;
  value: float; 
  day : string;
  dates: string list;
}

(** Raised when a trade is invalid for various reasons *)
exception Broke

(** Raised when the simulation reaches the end date *)
exception EndOfSim of state

(** [next state stocks steps] advances the simulation by [steps] days, returning
    a new state with the values updated as necessary. 
    Raises [EndOfSim] if the incrementation of the date would cause the 
    simulation to go over or reach the end date *)
val next : state -> stock list -> int -> state

(** [buy state stocks ticker amount] executes a purchase of [ticker] with
    [amount] shares, returning a new state with the values updated as necessary.
    Raises [Broke] if the price of the trade is greater than the 
    user's balance or the ticker is invalid*)
val buy : state -> stock list -> string -> int -> state 

(** [sale state stocks ticker amount] executes a sale of [ticker] with
    [amount] shares, returning a new state with the values updated as necessary.
    Raises [Broke] if the user does not own enough of the specified stock
    or if the ticker is invalid*)
val sell : state -> stock list -> string -> int -> state 

(** TODO DOCUMENT *)
val short : state -> stock list -> string -> int -> state

(** TODO DOCUMENT *)
val close : state -> stock list -> string -> string -> state

val update_val : (string * int) list -> (string * string * int) list ->
  string -> Scraper.stock list -> float
  
val get_ticker : stock list -> string -> stock
