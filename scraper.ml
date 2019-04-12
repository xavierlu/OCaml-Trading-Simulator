open Unix

type stock = {
  ticker : string;
  open_prices : float list;
  close_prices : float list;
}

(** [file_crawler file filename] returns a stock type that contains the 
    pertinent information to the stock contained in [file]. The argument 
    [filename] exists to make parsing the ticker easier *)
let file_crawler file filename = 
  let separator = ',' in
  let rec crawler file (op: float list) (cp: float list) = 
    try
      let next_line = input_line file in
      let separated = String.split_on_char separator next_line in
      crawler file (float_of_string (List.nth separated 2)::op) 
        (float_of_string (List.nth separated 5)::cp)
    with _ ->
      let t_length = (String.index filename '.') - 6 in
      let t = String.uppercase_ascii (String.sub filename 6 t_length) 
      in {ticker = t; open_prices = List.rev op; close_prices = List.rev cp}
  in
  crawler file [] []

(** [ticker_cmp s1 s2] compares stocks [s1] and [s2] accoriding to the 
    alphabetical order of their tickers *)
let ticker_cmp (s1:stock) (s2:stock) = 
  compare s1.ticker s2.ticker

(** [directory_crawler dir path list] calls [file_crawler] on every .csv file
    in [dir], returning a lists of stock types*)
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