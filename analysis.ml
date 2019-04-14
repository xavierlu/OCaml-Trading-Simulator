open Scraper

(** [momentum] calcultes the change in closing stock price
    between the current day price and the price n-days
    prior. 
    Requires: last entry in [stock] is current day*)
let momentum stock n = 
  let prices = stock.close_prices in 
  let len = List.length prices in
  match prices with 
    | [] -> failwith "error"
    | h::t -> List.nth prices len -. (List.nth prices (len - n))


let rate_of_change stock n = 
  let prices = stock.close_prices in 
  let len = List.length prices in
  let momentum = momentum stock n in
  momentum /. (List.nth prices (len - n))

let rec sma_helper lst n i acc = 
  match lst with 
  | [] -> acc
  | h::t -> if n = i then acc else sma_helper t n (i+1) (acc +. h)

let sma stock n = (sma_helper stock.close_prices n 0 0.0) /. float_of_int n

(** [get_mean] returns mean value of prices in 
  stock_prices list *)
let rec get_mean (stock_prices : float list) n = 
     match stock_prices with
     |[] -> 0.0
     |h::t -> h /. (float_of_int n) +. get_mean t n 

(** [vol] calculates the standard deviation, or 
    volatility, of the stock over n days *)
let vol stock n = 
     let price_length = List.length stock.close_prices in 
     let price_mean = get_mean stock.close_prices price_length  in
     let rec get_stdev prices n = 
        match prices with
        |[] -> 0.0
        |h::t -> (h -. price_mean)**2.0 /. (float_of_int (n-1)) +. 
                  get_stdev t n in
      sqrt (get_stdev stock.close_prices price_length)

(** [skew] calculates the skewness of the price data for [stock].
    skew >> 0 -> price positively skewed
    skew << 0 -> price negatively skewed
    skew ~ 0 -> price is normally distributed *)
let skew stock = 
    let sorted_prices =  List.sort compare stock.close_prices in 
    let len = List.length stock.close_prices in 
    let median = if len mod 2 = 1 then List.nth sorted_prices ((len-1)/2)
                 else ((List.nth sorted_prices (len/2)) +. (List.nth sorted_prices (len/2 -1))) /. 2.0
                 in
    let mean = get_mean stock.close_prices len in
    let stdev = vol stock len in 
    (3.0*.(mean -. median)) /. stdev
       
