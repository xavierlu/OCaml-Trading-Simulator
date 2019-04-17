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

let rate_of_change_test (name:string) (stk:stock) (n:int) (expected:float): 
  test = 
  name >:: (fun _ -> assert_equal (rate_of_change stk n) expected)

let sma_test (name:string) (stk:stock) (n:int) (expected:float): test = 
  name >:: (fun _ -> assert_equal (sma stk n) expected)

let get_mean_test (name:string) (prices:float list) (n:int) (expected:float):
  test = 
  name >:: (fun _ -> assert_equal (get_mean prices n) expected)

let vol_test (name:string) (stk:stock) (n:int) (expected:float): test = 
  name >:: (fun _ -> assert_equal (vol stk n) expected)

let skew_test (name:string) (stk:stock) (expected:float): test = 
  name >:: (fun _ -> assert_equal (skew stk) expected)


(** [scraper_tests] is a list of tests for frunctions in scraper.ml*)
let scraper_tests =
  let file = open_in ("quantquote_small/table_aapl.csv") in
  let goog = file_crawler file "table_goog.csv" in
  let ba = file_crawler file "table_ba.csv" in
  let aapl = file_crawler file "table_aapl.csv" in

  [
    ticker_cmp_test "ticker_cmp_1" goog ba (-1);
    ticker_cmp_test "ticker_cmp_2" aapl ba 1;
    ticker_cmp_test "ticker_cmp_3" ba ba 0;
  ]

let analysis_tests = 
  let file = open_in ("quantquote_small/table_aapl.csv") in
  let goog = file_crawler file "table_goog.csv" in
  let ba = file_crawler file "table_ba.csv" in
  let aapl = file_crawler file "table_aapl.csv" in
  [
    momentum_test "momentum_test_1" aapl 9 0.0; 
    momentum_test "momentum_test_2" goog 0 0.0;
    momentum_test "momentum_test_3" ba 2 0.0;

    (*rate_of_change_test "rate_test_1"
      rate_of_change_test "rate_test_2"
      rate_of_change_test "rate_test_3"

      sma_test "sma_test_1"
      sma_test "sma_test_2"
      sma_test "sma_test_3"

      get_mean_test "get_mean_test_1"
      get_mean_test "get_mean_test_2"
      get_mean_test "get_mean_test_3"

      vol_test "vol_test_1"
      vol_test "vol_test_2"
      vol_test "vol_test_3"

      skew_test "skew_test_1"
      skew_test "skew_test_2"
      skew_test "skew_test_3"*)
  ]

let command_tests =
  []

let trade_tests =
  []

let tests =
  "test suite for A6"  >::: List.flatten
    [
      scraper_tests;
      analysis_tests;
      command_tests;
      trade_tests;
    ]

let _ = run_test_tt_main tests
