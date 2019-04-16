open Scraper

type state = {
  balance: float;
  portfolio: (string * int) list;
  value: float; 
  day : int
}

val buy : state -> stock list -> string -> int -> state 

val sell : state -> stock list -> string -> int -> state 

