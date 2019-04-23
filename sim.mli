open Trade
open Command
open Scraper

(** [rule] represents a valid trading rule. Everything is straightfoward,
    "all" is represented as [amt] = -1 *)
type rule = {
  verb: string;
  ticker: string;
  amt: int;
  freq: string;
  measure: string; (*measure type? *)
  argument: int; (* the args that needed to pass into the measure analysis *)
  gtlt: char;
  number: float;
}

val evaluate_measure : string -> int -> Scraper.stock -> Trade.state -> float
