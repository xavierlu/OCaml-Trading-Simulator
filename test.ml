open OUnit2
open Scraper
open Analysis
open Trade

(******************************************************************************
   Helper functions for testing
 ******************************************************************************)

let ticker_cmp_test (name:string) (s1:stock) (s2:stock) (expected:int): test =
  name >:: (fun _ -> assert_equal (ticker_cmp s1 s2) expected)

let momentum_test (name:string) (stk:stock) (n:int) (expected:float): test = 
  name >:: (fun _ -> assert_equal (momentum stk n) expected)

let rate_of_change_test (name:string) (stk:stock) (n:int) (expected:float): test = 
  name >:: (fun _ -> assert_equal (rate_of_change stk n) expected)

let sma_test (name:string) (stk:stock) (n:int) (expected:float): test = 
  name >:: (fun _ -> assert_equal (sma stk n) expected)

let get_mean_test (name:string) (prices:float list) (n:int) (expected:float):
  test = 
  name >:: (fun _ -> assert_equal (get_mean prices n) expected)

let vol_test (name:string) (stk:stock) (n:int) (expected:float): test = 
  name >:: (fun _ -> assert_equal (vol stk n) expected)

let skew_test (name:string) (stk:stock) (expected:float): test = 
  name >:: (fun _ -> 
      assert_equal (momentum stk) expected)

let test_parse (name:string) (command : string) 
    () : bool = 
  let stock =  in