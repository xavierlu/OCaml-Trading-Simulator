open Scraper

(** [sublist list current_day] returns a sublist [list] from the first day up 
    and until the current_day*)
let sublist list current_day = 
  let rec sublist_helper lst acc = 
    match lst with 
    | [] -> acc
    | h::t when int_of_string (fst h) >= int_of_string current_day -> acc@[h]
    | h::t -> sublist_helper t (acc@[h]) in
  sublist_helper list []

(** [momentum] calcultes the change in closing stock price
    between the current day price and the price n-days
    prior. 
    Requires: last entry in [stock] is current day*)
let momentum stock n current_day = 
  let prices = sublist stock.close_prices current_day in
  let len = (List.length prices) - 1 in
  match prices with 
  | [] -> failwith "error"
  | h::t -> snd (List.nth prices len) -. snd (List.nth prices (len - n))


let rate_of_change stock n current_day = 
  let prices = sublist stock.close_prices current_day in
  let len = (List.length prices) - 1 in
  let momentum = momentum stock n current_day in
  momentum /. snd (List.nth prices (len - n))


let sma stock n current_day = 
  let rec sma_helper lst n i acc = 
    match lst with 
    | [] -> acc
    | h::t when n = i -> acc
    | h::t -> if int_of_string (fst h) < int_of_string current_day then 
        sma_helper t n (i+1) (acc +. snd h) else sma_helper t n i acc in
  (sma_helper (List.rev stock.close_prices) n 0 0.0) /. float_of_int n

(** [get_mean] returns mean value of prices in 
    stock_prices list *)
let rec get_mean stock_prices n = 
  match stock_prices with
  |[] -> 0.0
  |h::t -> snd h /. (float_of_int n) +. get_mean t n 

(** [vol] calculates the standard deviation, or 
    volatility, of the stock over n days *)
let vol stock n current_day = 
  let new_list = sublist stock.close_prices current_day in
  let price_mean = get_mean new_list n in
  let rec get_stdev prices n =  
    match prices with
    | [] -> 0.0
    | h::t -> (snd h -. price_mean)**2.0 /. (float_of_int (n-1)) +. 
              get_stdev t n in
  sqrt (get_stdev new_list n)

(**[just_priced priceData] returns a list of the second elements of each tuple
   in [priceData] *)
let rec just_prices priceData = 
  match priceData with
  |[] -> []
  | h::t -> (snd h)::(just_prices t)

(** [skew] calculates the skewness of the price data for [stock].
    skew >> 0 -> price positively skewed
    skew << 0 -> price negatively skewed
    skew ~ 0 -> price is normally distributed *)
let skew stock current_day = 
  let new_list = sublist stock.close_prices current_day in
  let sorted_prices =  List.sort compare (just_prices new_list) in 
  let len = List.length new_list in 
  let median = if len mod 2 = 1 then List.nth sorted_prices ((len-1)/2)
    else ( List.nth sorted_prices (len/2) 
           +. List.nth sorted_prices (len/2 -1)) /. 2.0
  in
  let mean = get_mean new_list len in
  let stdev = vol stock len current_day in 
  (3.0*.(mean -. median)) /. stdev

