(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nucleotides.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/18 22:36:08 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/18 22:36:09 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide c =
   let gene car = match car with
   | 'a' | 'A' -> A
   | 't' | 'T' -> T
   | 'c' | 'C' -> C
   | 'g' | 'G' -> G
   | _ -> None
   in let (nuc:nucleotide) = ("phosphate", "deoxyribose", (gene c))
   in nuc

let test_nuc nuc =
   let nucleo c = match c with
   | A -> print_char ('A'); print_char '\n'
   | T -> print_char ('T'); print_char '\n'
   | C -> print_char ('C'); print_char '\n'
   | G -> print_char ('G'); print_char '\n'
   | _ -> ()
   in let end_nuc n = match n with
   | (a, b, d) -> print_endline a; print_endline b; (nucleo d)
   in end_nuc nuc

let () =
   test_nuc ((generate_nucleotide 'A'));
   print_char '\n';
   test_nuc ((generate_nucleotide 'T'));
   print_char '\n';
