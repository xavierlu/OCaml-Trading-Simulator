type phrase = string list 

type command = 
<<<<<<< HEAD
    | Buy of phrase
    | Sell of phrase
    | Quit 
    | Volatility of phrase
    | SMA of phrase 
    | Skew of phrase 
    | Analysis of phrase
    | Next of phrase
=======
  | Buy of phrase
  | Sell of phrase
  | Quit 
  | Help
  | View
  | Volatility of phrase
  | Next of phrase
>>>>>>> a428794667eb8033ca7d709963c413849d1556b7

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse string] parses the inputted [string] and returns the
    corresponding command*)
val parse: string -> string -> command