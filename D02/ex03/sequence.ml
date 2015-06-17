let rec list_to_string liste = match liste with
    | [] -> "\n"
    | hd::tl -> (string_of_int hd) ^ (list_to_string tl)

let encode n =
    let rec test a acc res =
      let loop liste =
         let rec do_work x tail acc =  match tail with
            | [] -> [acc; x]
            | hd::tl -> if hd = x then do_work x tl (acc + 1) else [acc; x] @ (do_work hd tail 0)
         in match liste with
            | [] -> []
            | hd::_ -> do_work hd liste 0
      in
      if (a = 0) then
         res ^ list_to_string acc
      else
          begin
             test (a - 1) (loop acc) (res ^ (list_to_string acc))
          end
   in
   if (n < 0) then
       ""
   else
       test n [1] ""


let() =
       print_endline (encode (-1));
       print_endline (encode 0);
       print_endline (encode 1);
       print_endline (encode 15)
