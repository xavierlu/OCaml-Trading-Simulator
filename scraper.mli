(** [stock] is the epresentation of a stock's historical data. Contains a 
    ticker symbol, a list of opening prices, and a list of closing prices *)
type stock = {
  ticker : string;
  open_prices : (string *float) list;
  high_prices: (string *float) list;
  low_prices: (string *float) list;
  close_prices : (string *float) list;
  volumes: (string *float) list;
  start_date: string;
}

(** [get_data path] crawls through the files in directory [path] 
    and outputs a [stock list] containing the stock data found in
    the directory
    Requires: csv files in the folder specified by [path] 
    have proper formatting and name formatting*)
val get_data : string -> stock list

(** [ticker_cmp s1 s2] compares stocks [s1] and [s2] accoriding to the 
    alphabetical order of their tickers *)
val ticker_cmp : stock -> stock -> int

(** [file_crawler file filename] returns a stock type that contains the 
    pertinent information to the stock contained in [file]. The argument 
    [filename] exists to make parsing the ticker easier *)
val file_crawler : in_channel -> string -> stock
