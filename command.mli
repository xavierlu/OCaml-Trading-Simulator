(** Representation of a phrase of extra information that may come after a 
    command verb*)
type phrase = string list 

(** Representation of a command that the user can input*)
type command = 
  | Buy of phrase
  | Sell of phrase
  | Quit 
  | Volatility of phrase
  | SMA of phrase 
  | Skew of phrase 
  | Momentum of phrase
  | ROC of phrase
  | Analysis of phrase
  | Next of phrase
  | Help 
  | View 
  | Price of phrase

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse string] parses the inputted [string] and returns the
    corresponding command*)
val parse: string -> Scraper.stock list -> command