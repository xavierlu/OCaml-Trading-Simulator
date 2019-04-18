open Scraper

type state = {
  balance: float;
  portfolio: (string * int) list;
  value: float; 
  day : string;
  dates: string list;
}

exception Broke

exception EndOfSim of state

val buy : state -> stock list -> string -> int -> state 

val sell : state -> stock list -> string -> int -> state 

val next : state -> stock list -> int -> state

