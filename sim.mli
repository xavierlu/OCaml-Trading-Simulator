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
  gtlt: char;
  number: float;
}

val evaluate_measure : string -> Scraper.stock -> Trade.state -> float
