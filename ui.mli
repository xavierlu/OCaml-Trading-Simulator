
(** [make state stocks bal portfolio short_positions value date dates] returns
    a [Trade.state] consisting of the relevant values. If [dates] is provided,
    the dates field of the returned state will equal [dates], otherwise [dates]
    will be calculated using [stocks]. *)
val make_state : Scraper.stock list -> float -> (string * int) list -> 
  (string * string * int) list -> float -> string -> string list -> Trade.state

(** [main] runs the ui for the interactive tool *)
val main : unit -> unit