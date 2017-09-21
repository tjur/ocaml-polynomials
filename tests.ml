open Polynomial.Polynomial;;


let count = ref 0;;

let assert_equal val1 val2 =
  count := !count + 1; let str = "#" ^ (string_of_int !count) ^ " "
  in if val1 = val2 then print_endline (str ^ "OK")
  else print_endline (str ^ "FAIL")

(* create, parse, to_string *)
let p1 = create [0.;0.;4.2; 3.; -13.56; 1.; 12.; -123.45];;
let p2 = parse "-x^2 + 4.2x^5 + 3x^4 - 13.56x^3 + 2x^2 + 12x - 123.45";;

assert_equal p1 p2;;
assert_equal (to_string p2) "4.2x⁵ + 3x⁴ - 13.56x³ + x² + 12x - 123.45";;
assert_equal (parse "-5x^12 + 1.2x^13 + 4x^12 - 1.1x^13 + 0x^20 - 0.1x^13") (parse "-x^12");;
assert_equal (to_string (parse "-3x^3 + 2.x^2 + x + 0")) ("-3x³ + 2x² + x");;

(* deg *)
assert_equal (deg (parse "x^2 + 2x + 1")) 2;;
assert_equal (deg (parse "-23x^50 + 30x^15 - 100x^10 - 4x + 20")) 50;;

(* calc *)
assert_equal (calc (parse "-23x^50 + 30x^15 - 100x^10 - 4x + 20") 0.) 20.;;
assert_equal (calc (parse "-23x^50 + 30x^15 - 100x^10 - 4x + 20") 1.) (-77.);;
assert_equal (calc (parse "5x^3 - 10x^2 + 3.5x + 27.4") 3.14) 94.58972;;

(* equal *)
assert_equal (equal (parse "x^2 + 2x + 1") (parse "x^2 + x + x + 0.6 + 0.4")) true;;
assert_equal (equal (parse "x^5") (parse "x^4")) false;;

(* get_factor *)
assert_equal (get_factor (parse "5x^3 - 10x^2 + 3.5x + 27.4") 2) (-10.);;
assert_equal (get_factor (parse "12x^30 + 3x + 27.4") 30) 12.;;
assert_equal (get_factor (parse "12x^30 + 3x + 27.4") 5) 0.;;
assert_equal (get_factor (parse "12x^30 + 3x + 27.4") 31) 0.;;

(* set_factor *)
let p1 = parse "12x^30 + 3x + 27.4";;
let p2 = (set_factor p1 100. 35);;
let p3 = set_factor (set_factor p1 0. 30) 20. 1;;

assert_equal (get_factor p2 35) 100.;;
assert_equal (deg p2) 35;;
assert_equal (get_factor p3 1) 20.;;
assert_equal (deg p3) 1;;

(* add, sub *)
let p1 = parse "x^2 + 2x + 1";;
let p2 = parse "x^3 + 2.5x^2 - x - 5";;
let p3 = parse "23.456x^7 - 3x^5 + 3.14x^4 + x^3 + 5x^2 + 100";;
let p4 = parse "x^10 + 7.544x^7 + 4.2x^5 - 1.13x^4 + x^3 + 99";;

assert_equal (equal (p1 +@ p2) (parse "x^3 + 3.5x^2 + x - 4")) true;;
assert_equal (equal (p3 +@ p4) (parse "x^10 + 31x^7 + 1.2x^5 + 2.01x^4 + 2x^3 + 5x^2 + 199")) true;;
assert_equal (equal (p1 -@ p2) (parse "-x^3 - 1.5x^2 + 3x + 6")) true;;
assert_equal (equal (p4 -@ p3) (parse "x^10 - 15.912x^7 + 7.2x^5 - 4.27x^4 - 5x^2 - 1")) true;;

(* mult *)
let p1 = parse "2x^3 - 2x^2 + x - 4";;
let p2 = parse "x^2 - 3x";;
let p3 = parse "4x^5 - 6x^2 + 3x";;
let p4 = parse "5x^4 - 2x^2 + 3";;
let p5 = parse "x^4 + 4x^2 - x + 2";;
let p6 = parse "x^4 + x^3 + 10";;

assert_equal (equal (p1 *@ p2) (parse "2x^5 - 8x^4 + 7x^3 - 7x^2 + 12x")) true;;
assert_equal (equal (p3 *@ p4) (parse "20x^9 - 8x^7 - 30x^6 + 27x^5 + 12x^4 - 6x^3 - 18x^2 + 9x")) true;;
assert_equal (equal (p5 *@ p6) (parse "x^8 + x^7 + 4x^6 + 3x^5 + 11x^4 + 2x^3 + 40x^2 - 10x + 20")) true;;

(* div *)
let p1 = parse "x^3 - 8x^2 + 15x - 8";;
let p2 = parse "x - 1";;
let (p3, p4) = p1 /@ p2;;
let (p5, p6) = p2 /@ p1;;
let p7 = ((parse "x^4 + 2x^3 - 3x + 1") *@ (parse "x^3 - 1")) +@ (parse "-x^2 - 1");;
let p8 = parse "x^3 - 1";;
let (p9, p10) = p7 /@ p8;;

assert_equal ((equal p3 (parse "x^2 - 7x + 8")) && (equal p4 (parse "0"))) true;;
assert_equal ((equal p5 (parse "0")) && (equal p6 (parse "x - 1"))) true;;
assert_equal ((equal p9 (parse "x^4 + 2x^3 - 3x + 1")) && (equal p10 (parse "-x^2 - 1"))) true;;

(* add_float, sub_float, mult_float, div_float *)
let p1 = parse "x^5 + x^4 + x^3 + x^2 + x + 1";;

assert_equal (equal (add_float p1 9.) (parse "x^5 + x^4 + x^3 + x^2 + x + 10")) true;;
assert_equal (equal (sub_float p1 1.) (parse "x^5 + x^4 + x^3 + x^2 + x")) true;;
assert_equal (equal (mult_float p1 (-3.)) (parse "-3x^5 - 3x^4 - 3x^3 - 3x^2 - 3x - 3")) true;;
assert_equal (equal (div_float p1 2.) (parse "0.5x^5 + 0.5x^4 + 0.5x^3 + 0.5x^2 + 0.5x + 0.5")) true;;

(* pow *)
let p1 = parse "x - 1";;
let p2 = parse "2x + 1";;

assert_equal (equal (p1 **@ 2) (parse "x^2 - 2x + 1")) true;;
assert_equal (equal (p2 **@ 3) (parse "8x^3 + 12x^2 + 6x + 1")) true;;

(* mult_fft *)
let p1 = parse "2x^3 - 2x^2 + x - 4";;
let p2 = parse "x^2 - 3x";;
let p3 = parse "4x^5 - 6x^2 + 3x";;
let p4 = parse "5x^4 - 2x^2 + 3";;
let p5 = parse "x^4 + 4x^2 - x + 2";;
let p6 = parse "x^4 + x^3 + 10";;

assert_equal (equal (p1 *@ p2) (parse "2x^5 - 8x^4 + 7x^3 - 7x^2 + 12x")) true;;
assert_equal (equal (p3 *@ p4) (parse "20x^9 - 8x^7 - 30x^6 + 27x^5 + 12x^4 - 6x^3 - 18x^2 + 9x")) true;;
assert_equal (equal (p5 *@ p6) (parse "x^8 + x^7 + 4x^6 + 3x^5 + 11x^4 + 2x^3 + 40x^2 - 10x + 20")) true;;

(* deriv *)
let p1 = parse "3x^5 - 7x^2 + 4x - 2";;
let p2 = parse "16x^4 - 23x^3 - 16x^2 + 2x + 9";;

assert_equal (equal (deriv p1) (parse "15x^4 - 14x + 4")) true;;
assert_equal (equal (deriv p2) (parse "64x^3 - 69x^2 - 32x + 2")) true;;

(* integral *)
let p1 = parse "15x^4 - 14x + 4";;
let p2 = parse "64x^3 - 69x^2 - 32x + 2";;

assert_equal (equal (integral p1) (parse "3x^5 - 7x^2 + 4x")) true;;
assert_equal (equal (integral p2) (parse "16x^4 - 23x^3 - 16x^2 + 2x")) true;;

(* definite_integral *)
let p1 = parse "15x^4 - 14x + 4";;
let p2 = parse "64x^3 - 69x^2 - 32x + 2";;

assert_equal (definite_integral p1 (3., -1.)) (692.);;
assert_equal (definite_integral p2 (0., 5.)) (-6735.);;

(* draw_polynomials *)
let p1 = parse "-x^2 + 0.5";;
let p2 = parse "20x^9 - 8x^7 - 30x^6 + 27x^5 + 12x^4 - 6x^3 - 18x^2 + 9x";;
let p3 = 
  mult_float ((parse "x + 50") *@ (parse "x + 30") *@ (parse "x + 15") *@ (parse "x") *@ (parse "x - 10") *@ (parse "x - 35")) 1000.;;
let p4 = ((parse "-x - 27") *@ (parse "x + 10") *@ (parse "x - 20") *@ (parse "x - 30") *@ (parse "x - 45"));;

draw_polynomials [((-1.,1.), p1); ((-20.,30.), p2); ((-55., 40.), p3); ((-35.,50.), p4)];;
