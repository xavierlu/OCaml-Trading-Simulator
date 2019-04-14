
(** [stock] is the epresentation of a stock's historical data. Contains a 
    ticker symbol, a list of opening prices, and a list of closing prices *)
type stock = {
  ticker : string;
  open_prices : float list;
  high_prices: float list;
  low_prices: float list;
  close_prices : float list;
  volumes: int list;
}

(** [get_data path] crawls through the files in directory [path] 
    and outputs a [stock list] containing the stock data found in
    the directory
    Requires: csv files in the folder specified by [path] 
    have proper formatting and name formatting*)
val get_data : string -> stock list