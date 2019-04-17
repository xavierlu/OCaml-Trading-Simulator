open Scraper

type state = {
  balance: float;
  portfolio: (string * int) list;
  value: float; 
  day : string;
  dates: string list;
}

val buy : state -> stock list -> string -> int -> state 

val sell : state -> stock list -> string -> int -> state 

val next : state -> stock list -> state

