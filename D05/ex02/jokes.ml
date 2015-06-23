(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: ael-kadh <ael-kadh@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/23 21:56:50 by ael-kadh          #+#    #+#             *)
(*   Updated: 2015/06/23 21:56:55 by ael-kadh         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let joks = 	[|"Quel super héros donne le plus vite l'heure ?\nSpeed heure man ! ( spider man )";
			"Qui du marin ou de l'aviateur écrit le moins ?\nRéponse : le marin car il a jeté l'ancre !";
			"Que se fait un Schtroumf quand il tombe ?\nUn bleu !";
			"Pourquoi Michael ouvre la porte ?\nParce que Jack sonne. (Jackson)";
			"Quelle est la femelle du hamster\nHamsterdame (Amsterdam)"|]

let main () =
	Random.self_init ();
	let nbJook = (Array.length joks) in
	let rd = (Random.int nbJook) in
	print_endline (joks.(rd))

let () = main ()
