(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   uncipher.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/19 23:21:42 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/19 23:22:13 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let uncaesar s i =
   Cipher.caesar s (-i)

let unrot42 s =
   uncaesar s (42)

let ft_uncrypt s l =
   let rec reverse_list l2 = match l2 with
      | [] -> []
      | hd::ln -> (reverse_list ln) @ [hd]
   in
   Cipher.ft_crypt s (reverse_list l)


   let main () =
      let s1 = "abcde" in
      let s2 = "coucou moi c'est AHMED" in
      let s2_b = "J'aime le caml (Je mens)" in
      let s3 = "Stern des SUEDEN" in

      let f1 s1 = Cipher.caesar s1 42 in
      let liste_f = ([f1;Cipher.rot42]) in
      let f2 s1 = uncaesar s1 42 in
      let liste_f2 = ([f2;unrot42]) in
      print_endline("ft_crypt avec caesar et rot42");
      print_string("Chaine envoyer = ");
      print_endline (s1);
      let ret1 = Cipher.ft_crypt s1 liste_f in
      print_endline (ret1);
      print_endline("Maintenant on decode!");
      print_endline (ft_uncrypt ret1 liste_f2);
      print_char '\n';
      (* ROT42 *)
      print_string ("Rot42 sur : ");
      print_endline(s2);
      let ret2 = Cipher.rot42 s2 in
      print_endline(ret2);
      print_endline ("Maintenant on decode");
      print_endline(unrot42 ret2);

      print_char '\n';

      (* CAESAR *)
      print_string ("Caesar de 6 sur : ");
      print_endline(s2_b);
      let ret2 = Cipher.caesar s2_b 6 in
      print_endline(ret2);
      print_endline ("Maintenant on decode");
      print_endline(uncaesar ret2 6);

      print_char '\n';

      (* XOR *)
      print_string ("XOR de 2 sur : ");
      print_endline(s3);
      let ret3 = Cipher.xor s3 2 in
      print_endline(ret3);
      print_string ("XOR de 5 sur : ");
      print_endline(s3);
      let ret4 = Cipher.xor s3 5 in
      print_endline(ret4)

   let () = main ()
