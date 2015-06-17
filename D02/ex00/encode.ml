let encode liste =
      let rec do_work x tail acc =  match tail with
         | [] -> [(acc, x)]
         | hd::tl -> if hd = x then do_work x tl (acc + 1) else [(acc, x)] @ (do_work hd tail 0)
         in
      match liste with
       | [] -> []
       | hd::_ -> do_work hd liste 0

let() =
   let get_1_0 (a, _) = a
      in let get_0_1 (_, b) = b
         in let rec test_encore liste =
         match liste with
            | [] -> ()
            | hd::tl -> print_int (get_1_0 hd ); print_char ' ';print_int( get_0_1 hd );print_char '\n'; test_encore tl
         in test_encore (encode [1;1;1;3;3;5;6;6;6;2]);
print_char '\n';
   let get_1_0 (a, _) = a
      in let get_0_1 (_, b) = b
         in let rec test_encore liste =
         match liste with
            | [] -> ()
            | hd::tl -> print_int(get_1_0 hd ); print_char ' ';print_string(string_of_bool( get_0_1 hd ));print_char '\n'; test_encore tl
      in test_encore (encode [true;false;false;false;true;true;true]);

print_char '\n';
   let get_1_0 (a, _) = a
      in let get_0_1 (_, b) = b
         in let rec test_encore liste =
            match liste with
               | [] -> ()
               | hd::tl -> print_int(get_1_0 hd ); print_char ' ';print_string( get_0_1 hd );print_char '\n'; test_encore tl
         in test_encore (encode ["coucou"; "salut"; "salut"; "toto"; "titi"; "titi"])
