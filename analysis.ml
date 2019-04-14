open Scraper

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

