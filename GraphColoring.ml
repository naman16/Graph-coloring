et printlist item_printer l =
   let rec printlist_aux l =
     match l with
     | [] -> Printf.printf "%c" ']'
     | (h::t) -> Printf.printf "%s" ", ";
                 item_printer h;
                 printlist_aux t
   in (Printf.printf "%c" '[';
       match l with
       | (h::t) -> item_printer h; printlist_aux t
       | _ -> printlist_aux l)

(* A function for displaying an integer item *)
let int_printer i = Printf.printf "%d" i

(* A function for displaying a colour (represented by a string) *)
let show_color c = Printf.printf "%s" c

(* A function for displaying a node, colour pair *)
let show_node_and_color (n,c) =
   Printf.printf "(%d," n; show_color c; Printf.printf ")"

(* A function for showing a (complete) colouring *)
let show_coloring l =
   Printf.printf "\nColoring for the graph: "; printlist show_node_and_color l;
   Printf.printf "\n"


exception Search_Failure

let ask_user printer config =
   printer config;
   Printf.printf "More solutions (y/n)? ";
   if (read_line () = "y")
   then (raise Search_Failure)
   else ()


let rec pick_color colors =
  match colors with
  |[] -> None
  |(h::t) -> Some (h,t)


let rec find_assoc_list key alist =
  match alist with
  | [] -> None
  | (k,d):: alist' -> if key = k then (Some d) else find_assoc_list key alist'



let rec check_clash color n colored =
  match n with
  |[]-> true
  |(h::t) -> match (find_assoc_list h colored) with
    |None -> check_clash color t colored
    |Some c -> if (color = c) then false else check_clash color t colored

      
let rec check_color node color adjacency colored =
  match (find_assoc_list node adjacency) with
  |None -> true
  |Some n -> check_clash color n colored



     
      
let color_graph nodes adjacency colors =
   let rec color_graph_aux nodes colored =
     let rec color_rest n ns col =
       match (pick_color col) with
       |None -> raise Search_Failure
       |Some(c,rcs) -> if (check_color n c adjacency colored)
	 then try (color_graph_aux ns ((n,c)::colored)) with
	   Search_Failure -> color_rest n ns rcs
	 else color_rest n ns rcs
     in match nodes with
     |[] -> ask_user show_coloring colored
     |(h::t) -> color_rest h t colors
   in try (color_graph_aux nodes []) with
        Search_Failure -> Printf.printf "\nNo (more) colourings possible\n"


