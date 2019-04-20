open Scraper

(** [momentum stock n] is the absolute difference between closing price
    [n] day ago and today's closing price of stock [stock]*)
val momentum : stock -> int -> string -> float

(** [rate_of_change stock n] is the rate of change between closing price
    [n] day ago and today's closing price of stock [stock]*)
val rate_of_change : stock -> int -> string -> float

(** [sma stock n] is the simple moving average between closing price
    [n] day ago and today's closing price of stock [stock]*)
val sma : stock -> int -> string -> float

(** [vol] calculates the standard deviation, or 
    volatility, of the stock over n days *)
val vol : stock -> int -> string -> float

(** [get_mean] returns mean value of prices in 
    stock_prices list *)
val get_mean : ('a * float) list -> int -> float

(** [skew] calculates the skewness of the price data for [stock].
    skew >> 0 -> price positively skewed
    skew << 0 -> price negatively skewed
    skew ~ 0 -> price is normally distributed *)
val skew : stock -> string -> float
