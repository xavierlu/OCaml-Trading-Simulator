open Scraper

(** [momentum stock n] is the absolute difference between closing price
    [n] day ago and today's closing price of stock [stock]*)
val momentum : stock -> int -> float

(** [rate_of_change stock n] is the rate of change between closing price
    [n] day ago and today's closing price of stock [stock]*)
val rate_of_change : stock -> int -> float

(** [sma stock n] is the simple moving average between closing price
    [n] day ago and today's closing price of stock [stock]*)
val sma : stock -> int -> string -> float

(** [vol] calculates the standard deviation, or 
    volatility, of the stock over n days *)
val vol : stock -> int -> float