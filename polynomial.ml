
module type POLYNOMIAL =
  sig

    type t

    val create : float list -> t
    val parse : string -> t

    val to_string : t -> string
    val print : t -> unit

    val deg : t -> int
    val calc : t -> float -> float
    val equal : t -> t -> bool
    val get_factor : t -> int -> float
    val set_factor : t -> float -> int -> t

    val add : t -> t -> t
    val sub : t -> t -> t
    val mult : t -> t -> t
    val div : t -> t -> t * t
    val add_float : t -> float -> t
    val sub_float : t -> float -> t
    val mult_float : t -> float -> t
    val div_float : t -> float -> t
    val pow : t -> int -> t
    val ( +@ ) : t -> t -> t
    val ( -@ ) : t -> t -> t
    val ( *@ ) : t -> t -> t
    val ( /@ ) : t -> t -> t * t
    val ( **@ ) : t -> int -> t
    
    val mult_fft : t -> t -> t
    val deriv : t -> t
    val integral : t -> t
    val definite_integral : t -> float * float -> float

    val draw : ?bounds: float * float -> t -> unit
    val draw_polynomials : ((float * float) * t) list -> unit
    
  end;;



module Polynomial : POLYNOMIAL = 
  struct

    type t = Poly of int * float list

    let reduce (Poly (deg, poly)) = 
      let rec aux deg poly = 
        if deg = 0 then Poly (deg, poly)
        else match poly with
          [] -> raise (Invalid_argument "Wrong polynomial factors")
        | x::xs -> if x <> 0. then Poly (deg, poly) else aux (deg - 1) xs
      in aux deg poly

    let remove_numerical_errors (Poly (deg, poly)) =
      Poly (deg, List.map (fun x -> if (abs_float x) < 1e-010 then 0. else x) poly)

    let create factors = 
      match factors with
        [] -> failwith "Empty factor list"
      | x::xs -> reduce (remove_numerical_errors (Poly ((List.length factors) - 1, factors)))

    let parse str =
      let trim_str = String.trim str
      in let str = 
        if trim_str = "" then failwith "Parse error" 
        else if trim_str.[0] = '-' then trim_str 
        else "+" ^ trim_str
      in let split str =
        let rec sign_pos i acc =
          if i = (String.length str) then List.rev acc
          else if str.[i] = '+' || str.[i] = '-' then sign_pos (i+1) (i::acc)
          else sign_pos (i+1) acc
        and split_aux sign_pos = 
          match sign_pos with
            [] -> [str]
          | _ -> 
            let rec aux sign_pos acc =
              match sign_pos with
                [] -> List.rev_map (fun s -> String.trim s) acc
              | [pos] -> aux [] ((String.sub str pos (String.length str - pos))::acc)
              | p1::p2::ps -> aux (p2::ps) ((String.sub str p1 (p2 - p1))::acc)
            in aux sign_pos []
        in split_aux (sign_pos 0 [])

      and to_pow_and_fact str =
        let (sign, str) =
          if str.[0] = '+' then (1., String.sub str 1 (String.length str - 1))
          else if str.[0] = '-' then (-1., String.sub str 1 (String.length str - 1))
          else failwith "Parse error"
        in let (pow_str, fact_str) =
          try let i = String.index str '^'
          in (String.trim (String.sub str (i+1) (String.length str - i - 1)), String.trim (String.sub str 0 i))
          with Not_found -> 
            let str = String.trim str
            in if str = "" then failwith "Parse error"
            else if str.[String.length str - 1] = 'x' then ("1",str)
            else ("0",str ^ "x")
        and to_power pow_str = 
          try let pow = int_of_string pow_str
          in if pow < 0 then failwith "Parse error" else pow
          with Failure _ -> failwith "Parse error"
        and to_factor fact_str =
          if fact_str.[String.length fact_str - 1] <> 'x' then failwith "Parse error"
          else 
            let fact_str = String.sub fact_str 0 (String.length fact_str - 1)
            in if fact_str = "" then 1.
            else try let fact = float_of_string fact_str
            in if fact < 0. then failwith "Parse error" else fact
            with Failure _ -> failwith "Parse error"
        in (to_power pow_str, sign *. (to_factor fact_str))

      and sum_factors pows_and_facts = 
        match pows_and_facts with
          [] -> failwith "Parse error"
        | _ -> 
          let rec aux pows_and_facts sum current_pow acc =
            match pows_and_facts with
              [] -> List.rev ((current_pow, sum)::acc)
            | (pow,fact)::xs ->
                if pow = current_pow then aux xs (sum +. fact) current_pow acc
                else aux xs fact pow ((current_pow, sum)::acc)
          in aux pows_and_facts 0. (fst (List.hd pows_and_facts)) []

      and create_poly pows_and_facts =
        let rec build_list n = 
          if n = 0 then []
          else 0.::(build_list (n-1))
        in let rec create_poly_aux pows_and_facts acc current_pow =
          match pows_and_facts with
            [] -> create acc
          | (pow,fact)::xs ->
              if pow = current_pow then create_poly_aux xs (fact::acc) (current_pow + 1)
              else create_poly_aux xs (fact::((build_list (pow - current_pow)) @ acc)) (pow + 1) 
        in create_poly_aux pows_and_facts [] 0

      in let pows_and_facts = List.sort (fun a b -> fst a - fst b) (List.map to_pow_and_fact (split str))
      in create_poly (sum_factors pows_and_facts)

    let to_string (Poly (deg, poly)) = 
      if deg = 0 && poly = [0.] then "0" 
      else 
        let number_to_power number = 
          let superscripts = ["⁰";"¹";"²";"³";"⁴";"⁵";"⁶";"⁷";"⁸";"⁹"]
          in let rec aux number acc = 
            if number = 0 then acc
            else aux (number / 10) ((List.nth superscripts (number mod 10)) ^ acc)
          in if number = 1 then ""
          else aux number ""

        in let factor_to_string deg factor = 
          if factor = 0. then ""
          else let str = string_of_float factor
            in let str = 
              if str = "1." then ""
              else if str = "-1." then "-"
              else if str.[(String.length str) - 1] = '.'
                then String.sub str 0 ((String.length str) - 1)
              else str
            in if deg = 0 
              then if factor = 1. then "1"
              else if factor = -1. then "-1"
              else str
            else str ^ "x" ^ (number_to_power deg)

        in let string_list = List.filter ((<>) "") (List.rev (List.mapi (factor_to_string) (List.rev poly)))
        in let fold_fun acc str = 
          if acc = "" then str
          else if str.[0] = '-'
            then acc ^ " - " ^ (String.sub str 1 ((String.length str) - 1))
          else acc ^ " + " ^ str
        in List.fold_left fold_fun "" string_list

      let print poly = 
        print_endline (to_string poly)

      let deg (Poly (degree, _)) = 
        degree

      let calc (Poly (_, poly)) x = 
        List.fold_left (fun acc fact -> (x *. acc) +. fact) 0. poly

      let equal poly1 poly2 = 
        to_string poly1 = to_string poly2

      let get_factor (Poly (deg, poly)) pow =
        if pow < 0 then failwith "Negative power"
        else if pow > deg then 0.
        else List.nth poly (deg - pow)

      let set_factor (Poly (deg, poly)) fact pow =
        if pow < 0 then failwith "Negative power"
        else if pow > deg
          then let rec build_list n = 
            if n = 0 then []
            else 0.::(build_list (n - 1))
          in create (fact::((build_list (pow - deg - 1)) @ poly))
        else
          create (List.mapi (fun i x -> if i = (deg - pow) then fact else x) poly)

      let rec zip f l1 l2 = 
        match l1, l2 with 
          [], [] -> []
        | x::xs, y::ys -> (f x y)::(zip f xs ys)
        | _ -> failwith "Different size of lists"

      let add (Poly (d1, p1)) (Poly (d2, p2)) = 
        let rec build_list n = 
          if n = 0 then []
          else 0.::(build_list (n - 1))
        in if d1 < d2
          then let l = (build_list (d2 - d1)) @ p1
          in reduce (Poly (d2, zip (+.) l p2))
        else
          let l = (build_list (d1 - d2)) @ p2
          in reduce (Poly (d1, zip (+.) p1 l))

      let sub (Poly (d1, p1)) (Poly (d2, p2)) = 
        let rec build_list n = 
          if n = 0 then []
          else 0.::(build_list (n - 1))
        in if d1 < d2
          then let l = (build_list (d2 - d1)) @ p1
          in reduce (Poly (d2, zip (-.) l p2))
        else
          let l = (build_list (d1 - d2)) @ p2
          in reduce (Poly (d1, zip (-.) p1 l))

      let mult (Poly (d1, p1)) poly2 = 
        let rec build_list n = 
          if n = 0 then []
          else 0.::(build_list (n-1))
        in let aux (Poly (d, poly)) deg factor = 
          Poly (d + deg, (List.map (( *. ) factor) poly) @ (build_list deg))
        in let poly_list = List.mapi (aux poly2) (List.rev p1)
        in List.fold_left add (create [0.]) poly_list

      let div poly1 poly2 =
        let (Poly (d2, p2)) = poly2
        in if p2 = [0.] then raise Division_by_zero
        else if (deg poly1) < (deg poly2) then (create [0.], poly1)
        else
          let rec build_list n = 
            if n = 0 then []
            else 0.::(build_list (n - 1))
          in let rec div_aux result remainder = 
            let (Poly (d, poly)) = remainder
            in let div_result = Poly (d - d2, (List.hd poly /. List.hd p2)::(build_list (d - d2)))
            in let new_result = add result div_result
            in let new_remainder = sub remainder (mult div_result poly2)
            in if new_remainder = (create [0.]) || (deg new_remainder) < (deg poly2)
              then (new_result, new_remainder)
            else 
              div_aux new_result new_remainder
          in div_aux (create [0.]) poly1

      let add_float poly value =
        add poly (create [value])

      let sub_float poly value =
        sub poly (create [value])

      let mult_float (Poly (deg, poly)) value =
        if value = 0. then Poly (0, [0.])
        else Poly (deg, List.map (( *. ) value) poly)

      let div_float (Poly (deg, poly)) value =
        if value = 0. then raise Division_by_zero
        else Poly (deg, List.map (fun x -> x /. value) poly)

      let pow poly n =
        if n < 0 then failwith "Negative exponent"
        else
          let rec aux_pow poly n =
            if n = 0 then create [1.]
            else if n mod 2 = 0 then aux_pow (mult poly poly) (n / 2)
            else mult poly (aux_pow (mult poly poly) (n / 2))
          in aux_pow poly n

      let ( +@ ) = 
        add

      let ( -@ ) = 
        sub

      let ( *@ ) = 
        mult

      let ( /@ ) = 
        div

      let ( **@ ) =
        pow

      let mult_fft poly1 poly2 = 
        let to_pow_of_two x = 
          let logarithm = (log (float_of_int x)) /. (log 2.)
          in int_of_float (2. ** (ceil logarithm))

        and increase_degree poly new_d =
          let (Poly (d, p)) = poly
          in if new_d <= d
            then poly
          else
            let rec build_list n = 
              if n = 0 then []
              else 0.::(build_list (n-1))
            in Poly (new_d, (build_list (new_d - d)) @ p)

        in let m = to_pow_of_two ((deg poly1) + (deg poly2) + 1)
        in let (poly1, poly2) = (increase_degree poly1 (m - 1), increase_degree poly2 (m - 1))
        in let rec fft factors m w =
          if m = 1
            then factors
          else
            let split arr =
              let rec aux lst even acc1 acc2 = 
                match lst with
                  [] -> (Array.of_list (List.rev acc1), Array.of_list (List.rev acc2))
                | x::xs -> 
                    if even then aux xs (not even) (x::acc1) acc2
                    else aux xs (not even) acc1 (x::acc2)
              in aux (Array.to_list arr) true [] []
            in let (even_fact, odd_fact) = split factors
            in let (f_even, f_odd) = (fft even_fact (m / 2) (Complex.mul w w), fft odd_fact (m / 2) (Complex.mul w w))
            in let f = Array.make m Complex.zero
            in let x = ref Complex.one
            in for i = 0 to (m / 2) - 1 do
              f.(i) <- Complex.add f_even.(i) (Complex.mul !x f_odd.(i));
              f.(i + (m / 2)) <- Complex.sub f_even.(i) (Complex.mul !x f_odd.(i));
              x := Complex.mul !x w;
            done; f;

        in let to_value_representation (Poly (d, p)) = (* przekształca wielomian do listy (d+1) wartości w punktach, będącymi pierwiastkami z jedności *)
          let pi = 3.14159265358979323846
          and float_m = float_of_int m
          in let w = {Complex.re = cos ((2. *. pi) /. float_m); im = sin ((2. *. pi) /. float_m)}
          in fft (Array.of_list (List.map (fun x -> {Complex.re = x; im = 0.}) p)) m w

        in let to_factor_representation arr =
          let pi = 3.14159265358979323846
          and float_m = float_of_int m
          in let w = {Complex.re = cos ((2. *. pi) /. float_m); im = sin ((2. *. pi) /. float_m)}
          in let fft_res = fft arr m (Complex.inv w)
          in let res = List.map (fun {Complex.re = r; _} -> r /. float_m) (Array.to_list fft_res)
          in let res2 = List.map (fun x -> if (abs_float x) < 1e-010 then 0. else x) res
          in let rev_res = List.rev res2 
          in create ((List.hd rev_res)::(List.rev (List.tl rev_res))) (* wsp. najw. potęgi jest na końcu, trzeba go przestawić na pocz. *)

        in let mult_arrays arr1 arr2 = 
          let len = Array.length arr1
          in let new_arr = Array.make len Complex.zero
          in for i = 0 to len - 1 do
            new_arr.(i) <- Complex.mul arr1.(i) arr2.(i)
          done; new_arr

        in let poly1_val_repr = to_value_representation poly1
        and poly2_val_repr = to_value_representation poly2
        in let result_poly_val_repr = mult_arrays poly1_val_repr poly2_val_repr
        in to_factor_representation result_poly_val_repr

      let deriv (Poly (deg, poly)) = 
        match (List.rev poly) with
          [] -> failwith "[]"
        | [x] -> Poly (0, [0.])
        | _::xs -> Poly (deg - 1, List.rev (List.mapi (fun n x -> float_of_int (n + 1) *. x) xs))

      let integral (Poly (deg, poly)) = 
        match (List.rev poly) with
          [] -> failwith "[]"
        | x::xs -> Poly (deg + 1, List.rev (0.::(List.mapi (fun deg fact -> fact /. (float_of_int (deg + 1))) (x::xs))))

      let definite_integral poly (a, b) = 
        let poly_int = integral poly
        in (calc poly_int a) -. (calc poly_int b)

      let rec draw ?(bounds = (-1.,1.)) poly = 
        let a = fst bounds
        and b = snd bounds
        in if a > b then draw ~bounds:(b,a) poly
        else
          let open Graphics
          in let init () = 
            open_graph "1300x800";
            set_window_title "Plot";

          in let round_to_int f = 
            int_of_float (floor (f +. 0.5))

          and round_float f = 
            if f < 0.
              then (ceil (f *. 100.)) /. 100.
            else
              (floor (f *. 100.)) /. 100.

          in let draw_plot () =
            let x_size = size_x ()
            and y_size = size_y ()
            and margin = 15
            in set_line_width 2;

            let rec calc_points value step acc =
              let new_acc = (value, calc poly value)::acc
              and new_value = value +. step
              in if new_value >= b then List.rev ((b, calc poly b)::acc)
              else calc_points new_value step new_acc

            in let get_min_max_y l =
              let rec aux l min max = 
                match l with
                  [] -> (min, max)
                | (_, y)::xs -> 
                    let new_min = if y < min then y else min
                    and new_max = if y > max then y else max
                    in aux xs new_min new_max
              in let v = snd (List.hd l)
              in aux (List.tl l) v v

            in let points = (calc_points a ((b -. a) /.  (float_of_int (x_size - 2 * margin))) [])
            in let (min_y, max_y) = get_min_max_y points

            in let map_fun (x, y) = 
              let new_x = (((x -. a) *. (float_of_int (x_size - (2 * margin)))) /. (b -. a)) +. (float_of_int margin)
              and new_y = (((y -. min_y) *. (float_of_int (y_size - (2 * margin)))) /. (max_y -. min_y)) +. (float_of_int margin)
              in (new_x, new_y)

            in let draw_axes () = 
              let x_axis = [(a, 0.);(b, 0.)]
              and y_axis = [(0., min_y);(0.,max_y)]
              in let screen_x_axis = 
                let tmp = (List.map (fun p -> let (_, new_y) = map_fun p in round_to_int new_y) x_axis)
                in Array.of_list [(0, List.nth tmp 0);(x_size, List.nth tmp 1)]
              and screen_y_axis = 
                let tmp = (List.map (fun p -> let (new_x, _) = map_fun p in round_to_int new_x) y_axis)
                in Array.of_list [(List.nth tmp 0, 0);(List.nth tmp 1, y_size)]
              in let arrow_x = 
                let (x, y) = screen_x_axis.(1)
                in Array.of_list [(x - 5, y + 5);(x, y);(x - 5, y - 5)]
              and arrow_y = 
                let (x, y) = screen_y_axis.(1)
                in Array.of_list [(x - 5, y - 5);(x, y);(x + 5, y - 5)]
              in let draw_lines () = 
                let x_zero = fst arrow_y.(1)
                and y_zero = snd arrow_x.(1)
                in draw_poly_line (Array.of_list [(margin, y_zero + 4);(margin, y_zero - 4)]);
                moveto (margin - 11) (y_zero - 20);
                draw_string (string_of_float (round_float a));
                draw_poly_line (Array.of_list [(x_size - margin, y_zero + 4);(x_size - margin, y_zero - 4)]);
                moveto (x_size - margin - 11) (y_zero - 20);
                draw_string (string_of_float (round_float b));
                draw_poly_line (Array.of_list [(x_zero - 4, margin);(x_zero + 4, margin)]);
                moveto (x_zero + 15) (margin - 6);
                draw_string (string_of_float (round_float min_y));
                draw_poly_line (Array.of_list [(x_zero - 4, y_size - margin);(x_zero + 4, y_size - margin)]);
                moveto (x_zero + 15) (y_size - margin - 6);
                draw_string (string_of_float (round_float max_y));

              in draw_poly_line screen_x_axis;
              draw_poly_line screen_y_axis;
              draw_poly_line arrow_x;
              draw_poly_line arrow_y;
              draw_lines ();

            in let screen_points = 
              Array.of_list (List.map (fun p -> let (new_x, new_y) = map_fun p in (round_to_int new_x, round_to_int new_y)) points)

            in set_color black;
            set_line_width 2;
            draw_axes ();
            set_color blue;
            draw_poly_line screen_points;

          in init (); draw_plot (); let _ = read_key () in ();;

      let rec draw_polynomials polys =
        match polys with
          [] -> ()
        | (bounds, poly)::ps ->
            draw ~bounds:bounds poly;
            Graphics.clear_graph ();
            draw_polynomials ps;

  end;;
