open Unix

type stock = {
  ticker : string;
  open_prices : (string * float) list;
  high_prices: (string * float) list;
  low_prices: (string * float) list;
  close_prices : (string * float) list;
  volumes: (string * float) list;
  start_date: string;
}

(** [file_crawler file filename] returns a stock type that contains the 
    pertinent information to the stock contained in [file]. The argument 
    [filename] exists to make parsing the ticker easier *)
let file_crawler file filename = 
  let separator = ',' in
  let rec crawler file (op: (string *float) list) (high: (string *float) list) 
            (low: (string *float) list) (cp: (string *float) list) (vol: (string *float) list) start_date int= 
    try
      let next_line = input_line file in
      let separated = String.split_on_char separator next_line in
      let date = List.nth separated 0 in
      crawler file ( (date, float_of_string (List.nth separated 2))::op) 
        ((date, float_of_string (List.nth separated 3))::high) 
        ((date, float_of_string (List.nth separated 4))::low) 
        ((date, float_of_string (List.nth separated 5))::cp) 
        ((date, float_of_string (List.nth separated 6))::vol)
        (if int = 0 then List.nth separated 0 else start_date)
        1
    with _ ->
      let t_length = (String.index filename '.') - 6 in
      let t = String.uppercase_ascii (String.sub filename 6 t_length) 
      in 
        { ticker = t; open_prices = List.rev op; 
          high_prices = List.rev high; low_prices = List.rev low; 
          close_prices = List.rev cp; volumes = List.rev vol;
          start_date = start_date }
  in
  crawler file [] [] [] [] [] "" 0 

(** [ticker_cmp s1 s2] compares stocks [s1] and [s2] accoriding to the 
    alphabetical order of their tickers *)
let ticker_cmp (s1:stock) (s2:stock) = 
  compare s1.ticker s2.ticker

(** [directory_crawler dir path list] calls [file_crawler] on every .csv file
    in [dir], returning a lists of stock types *)
let rec directory_crawler dir path lst = 
  try
    let next_file_name = readdir dir in
    if Filename.check_suffix next_file_name ".csv" then
      let file = open_in (path ^ "/" ^ next_file_name) in
      let stock = file_crawler file next_file_name in
      directory_crawler dir path (stock::lst)
    else directory_crawler dir path lst
  with _ -> List.sort ticker_cmp lst 

let get_data (path : string) =
  let dir = opendir path in 
  directory_crawler dir path []