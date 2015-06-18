(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   rna.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 22:36:21 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/18 22:36:22 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide = phosphate * deoxyribose * nucleobase
type helix = nucleotide list
type rna = nucleobase list

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



let rec generate_rna (l:helix) =
   let l2 = (complementary_helix l) in
      let rec choose_nuc liste = match liste with
         | (a, b, c)::tl when c = T -> [U] @ (choose_nuc tl)
         | (a, b, c)::tl -> [c] @ (choose_nuc tl)
         | [] -> []
      in (choose_nuc l2:rna)

let rec test_rna l = match l with
   | hd::tl when hd = A -> print_string "A"; test_rna tl
   | hd::tl when hd = T -> print_string "T"; test_rna tl
   | hd::tl when hd = C -> print_string "C"; test_rna tl
   | hd::tl when hd = G -> print_string "G"; test_rna tl
   | hd::tl when hd = U -> print_string "U"; test_rna tl
   | _ -> print_char '\n'

let () =
   test_rna(generate_rna [("","", A);("","",T);("","",C);("","",G);("","",A)]);
   test_rna(generate_rna (generate_helix ~-1));
   test_rna(generate_rna (generate_helix 0));
   test_rna(generate_rna (generate_helix 4));
   test_rna(generate_rna (generate_helix 100));
