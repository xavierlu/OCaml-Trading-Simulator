type phrase = string list 

type command = 
    | Buy of phrase
    | Sell of phrase
    | Quit 
    | Volatility of phrase
    | SMA of phrase 
    | Skew of phrase 
    | Analysis of phrase
    | Next of phrase

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse string] parses the inputted [string] and returns the
  corresponding command*)
val parse: string -> command