(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   helix.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 22:36:15 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/18 22:36:16 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = phosphate * deoxyribose * nucleobase
type helix = nucleotide list

let generate_nucleotide c =
   let gene car = match car with
   | 'a' | 'A' -> A
   | 't' | 'T' -> T
   | 'c' | 'C' -> C
   | 'g' | 'G' -> G
   | _ -> None
   in let (nuc:nucleotide) = ("phosphate", "deoxyribose", (gene c))
   in nuc

let generate_nucleotide_int c =
      let gene car = match car with
      | 0 -> A
      | 1 -> T
      | 2 -> C
      | 3 -> G
      | _ -> None
      in let (nuc:nucleotide) = ("phosphate", "deoxyribose", (gene c))
      in nuc

let rec complementary_helix (l:helix) =
   let do_it h = match h with
         | (a, b, c)::tl when c = A -> [(a, b, T)] @ complementary_helix tl
         | (a, b, c)::tl when c = T -> [(a, b, A)] @ complementary_helix tl
         | (a, b, c)::tl when c = C -> [(a, b, G)] @ complementary_helix tl
         | (a, b, c)::tl when c = G -> [(a, b, C)] @ complementary_helix tl
         | _::_ -> []
         | [] -> []
      in let (var:helix) = (do_it l)
      in var

let generate_helix n =
   Random.self_init ();
   let rec loop a (liste:helix) =
      if (a > 0) then
         loop (a - 1) (generate_nucleotide_int(Random.int 4)::liste)
      else
         liste
   in loop n []

let tuples_to_string (t:nucleotide) = match t with
   | (a, b, c) when c = A -> "A"
   | (a, b, c) when c = T -> "T"
   | (a, b, c) when c = C -> "C"
   | (a, b, c) when c = G -> "G"
   | _ -> ""

let rec helix_to_string (l:helix) = match l with
   | td::tl -> (tuples_to_string td) ^ (helix_to_string tl)
   | [] -> ""

(* let test_nuc nuc =
   print_char '[';
   let nucleo c = match c with
   | A -> print_char ('A'); print_char ' '
   | T -> print_char ('T'); print_char ' '
   | C -> print_char ('C'); print_char ' '
   | G -> print_char ('G'); print_char ' '
   | _ -> ()
   in let end_nuc n = match n with
   | (a, b, d) -> print_string a; print_char ' '; print_string b; print_char ' '; (nucleo d)
   in    end_nuc nuc; print_string "] "

let rec t_nuc l = match l with
   | hd::tl -> test_nuc hd; t_nuc tl
   | [] -> print_char '\n' *)

let () =
   print_endline "Helix and complementary helix";
   let test = generate_helix 3 in
   print_endline(helix_to_string test);
   print_endline(helix_to_string (complementary_helix test));
   print_endline "Helix 2";
   print_endline(helix_to_string (generate_helix 2));
   print_endline "Helix -1";
   print_endline(helix_to_string (generate_helix ~-1));
   print_endline "Helix 0";
   print_endline(helix_to_string (generate_helix 0));
   print_endline "Helix 10";
   print_endline(helix_to_string (generate_helix 10));
   print_endline "Helix 100";
   print_endline(helix_to_string (generate_helix 100))
