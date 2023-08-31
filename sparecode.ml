(***********************************)
(* let bindings don't mutate stuff *)

let x = 1
let print_x_first_version () = print_endline ("what is our first x? is it *still* 1? \t" ^  (string_of_int  x))

let x = x + 1
let print_x_second_version () = print_endline ("what is our second x? \t\t\t" ^ string_of_int  x)

(* 'let () = ...' is just a way of getting the output, making sure it's (), then throwing it away *)
let () = print_endline "----" 

let () = print_x_second_version ()
let () = print_x_first_version ()


let () = print_endline "*******"
(*********************************)
(* why don't they mutate stuff?? *)


let show_unrolled () =
    let x = 1 in
        let print_x_first_version () = print_endline ("what is our first x? is it *still* 1? \t" ^  (string_of_int  x)) in
            (* what scope are we in here?  so what's x on the line below? *)
            let x = x + 1 in 
                (* what scope are we in here?  so what's x on the line below this? *)
                let print_x_second_version () = print_endline ("what is our second x? \t\t\t" ^ string_of_int  x) in
                    let () = print_endline "----" in
                    
                        let () = print_x_second_version () in
                            let () = print_x_first_version () in 
                                ()

let () = show_unrolled ()


let () = print_endline "*******"
(******************************)
(* statements *)


let statement_example_first_version () =
    print_endline "hi";
    print_endline "bye"

let statement_example_second_version () = 
    let () = print_endline "hi" in
        print_endline "bye"


let () = statement_example_first_version ()
let () = statement_example_second_version ()



let () = print_endline "*******"
(******************************)
(* another shadowing example *)

let y = 10

let shadowing_test =
    (let y = 2 in 
        Printf.printf "%i\n" y;
        (let y = 4 in 
            Printf.printf "%i\n" y);
        Printf.printf "%i\n" y;
        98)
        (* side note: shadowing_test is not a function!  it's just a nickname for 98 *) 
        (* when we evaluate a let expression, the body determines the value of the let expression *)         

let () = Printf.printf "%i\n" y


let () = print_endline "*******"
(******************************)
(* evaluating to 'multiple values' --- just a tuple *)


let plusminus a b =
  a + b, a - b 
  
let plusitem, minusitem = plusminus 10 20 
let () = print_endline (string_of_int plusitem)
let () = print_endline (string_of_int minusitem)

let plusminustimes a b =
  a + b, a - b , a * b

let plusitem, minusitem, timesitem = plusminustimes 10 20
let () = print_endline (string_of_int plusitem)
let () = print_endline (string_of_int minusitem)
let () = print_endline (string_of_int timesitem)