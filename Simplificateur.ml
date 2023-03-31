(* 
   Membres du groupe :
      - KESSOURI Lyna 
      - HO Nguyen Quynh Nhu
      - IRCA Jayson  
 *)


(* QUESTION 01 : 
               ANALYSE *)


(* type tree *)

type op =
  Plus | Moins | Multiple | Div
;;

type tree =
  | Var of char
  | Cst of int
  | Minus of tree
  | Node2 of op * tree * tree
;;

(* fonctions utiles *)

(* 
     - fonction qui verifie si le string pass� 
     en paramatre est un entier positif ou pas. 
     - version en utlisant une fonctionnelle faite mais qui s'ex�cute pas 
     avec notre version d'ocaml sur Emacs (elle est mise entre parenth�se 
     - version utlisant une boucle for 
*)

let  isCst(s : string) : bool=
  let result : bool ref = ref (s.[0] >= '0' && s.[0] <='9')  in
  for i = 0 to String.length(s)-1
  do
    if s.[i] >= '0' && s.[i] <='9'
    then
      result := !result 
    else
      result := false
  done;
  !result
;;

(* --- version pas utlise mais en fonctionnelle 
       let isPos n = (n>='0') && (n<='9') ;;

       let str2charList(s : string) =
         String.fold_left(fun acc c -> acc@[c])[]s
       ;;

       let isPosStr(s : string) : bool =
           List.map(isPos)s
       ;;
*)

(* fonction qui verifie si le string passe en parametre est bien une lettre miniscule *) 
let isVar(s : string) : bool =
  (String.length s = 1) &&(s.[0] >= 'a') && (s.[0] <= 'z')
;;

(* fonction parse *)
let parse(listeStr : string list) : tree =
  let rec parse_aux(stackTree, lstr : tree list * string list) : tree =
    match lstr with 
    | [] -> 
      (
        match stackTree with 
        | [] -> failwith("erreur : liste incoh�rente ")
        | [tree] -> tree
        | _ -> failwith(" erreur : liste incoh�rente") 
      ) 
    | x::tl -> 
       (
         if isCst(x)
         then parse_aux(Cst(int_of_string x)::stackTree,tl)
         else
           if isVar(x)
           then parse_aux(Var(String.get x 0)::stackTree, tl)
           else
             match x with 
             | "+" ->
                (
                  match stackTree with
                  |th1::th2::st -> parse_aux(Node2(Plus, th2, th1)::st, tl) 
                  | _ -> failwith("erreur : liste incoh�rente")
                )
             | "-" ->
                (
                  match stackTree with 
                  |th1::th2::st -> parse_aux(Node2(Moins, th2, th1)::st, tl) 
                  | _ -> failwith("erreur : liste incoh�rente")
                )
             | "*" ->
                (
                  match stackTree with 
                  |th1::th2::st -> parse_aux(Node2(Multiple, th2, th1)::st, tl) 
                  | _ -> failwith("erreur : liste incoh�rente")
                )
             | "/" ->
                (
                  match stackTree with 
                  |th1::th2::st -> parse_aux(Node2(Div, th2, th1)::st, tl) 
                  | _ -> failwith("erreur  : liste incoh�rente ") 
                ) 
             | "~" ->
                (
                  match stackTree with 
                  |th1::th2::st -> parse_aux(Minus(th1)::th2::st, tl) 
                  | _ -> failwith("erreur : liste incoh�rente")
                ) 
             | _  -> failwith("erreur : liste incoh�rente ")
       )
  in parse_aux([], listeStr)
;;


(* QUESTION 02 : 
      SIMPLIFICATION DES EXPRESSIONS 

      NOTE : la division de deux constantes a et b renvoie uen constante 
             meme si a est inferieur a b (division entiere) 
 *)


(* fonction que transforme chaque op�rateur de type op en l'operation qui le concerne *)
let opFonction operateur =
  match operateur with
  | Plus -> ( + )
  | Moins -> ( - )
  | Div -> ( / )
  | Multiple -> ( * )
;;

(* fonction auxiliere pour la simplification *) 
let simplification_aux(arbre : tree) : tree =
    match arbre with
    | Cst(x) -> Cst(x)
    | Var(x) -> Var(x)
                
    | Node2(op,Cst(x),Cst(y)) -> if (x < y) && (op = Moins)
                                 then Minus(Node2(Moins,Cst(y),Cst(x)))
                                 else Cst((opFonction op) x y)
                               
    | Node2(Plus,Cst(0),Var(x)) -> Var(x)
    | Node2(Plus,Var(x),Cst(0)) -> Var(x)

    | Node2(Moins,Var(x),Var(y)) -> if x = y
                                    then Cst(0)
                                    else arbre
                                  
    | Node2(Multiple,Var(x),Cst(0)) -> Cst(0)
    | Node2(Multiple,Cst(0),Var(x)) -> Cst(0)

    | Node2(Multiple,Var(x), Cst(1)) -> Var(x)
    | Node2(Multiple,Cst(1),Var(x)) -> Var(x)

    | Node2(Div,Var(x),Var(y)) -> if x = y
                                  then Cst(1)
                                  else arbre
    | _ -> arbre 
         
;;

(* fonction de la simplification *)
let rec simplification(arbre : tree) : tree =
  let result : tree = simplification_aux(arbre) in 
  match result with
  | Minus(x) -> Minus(simplification(x))
  | Node2(op,left,right) -> simplification_aux(Node2(op,simplification(left),simplification(right)))
  | _ -> result
 ;;

(* QUESTION 03 : 
     AFFICHAGE DU RESULTAT 
*)

let rec affichage(tree : tree) : string =
   match tree with
   |Cst(x) -> string_of_int(x)
   |Var(x) -> String.make 1 x
   |Minus(x) -> "( ~ " ^ affichage(x) ^ " )"
   |Node2(Plus, x, y) ->
     (
       match x , y with
       |Node2(op1,_,_) , Node2(op2,_,_) ->
         (
           match op1, op2 with
           |Plus, Plus -> affichage(x) ^ " + " ^ affichage(y)
           |_ , Plus -> "( " ^ affichage(x) ^ " ) + " ^ affichage(y) 
           |Plus, _ -> affichage(x) ^ " + ( " ^ affichage(y) ^ " )"
           |_, _ ->   affichage(x) ^ " + " ^ affichage(y) 
         )
       |Node2(op1 ,_,_),_ -> if not( op1 = Plus)
                              then "( " ^ affichage(x) ^ " ) + " ^ affichage(y)
                              else  affichage(x) ^ " + " ^ affichage(y)
       |_,Node2(op1,_,_) -> if not(op1 = Plus)
                             then   affichage(x) ^ " + ( " ^ affichage(y) ^ " )"
                             else  affichage(x) ^ " + " ^ affichage(y)
       |_,_ ->  affichage(x) ^ " + " ^ affichage(y)
     )
   |Node2(Moins, x , y)  ->
     (
       match x , y with
       |Node2(op1,_,_) , Node2(op2,_,_) ->
         (
           match op1, op2 with
           |Moins, Moins -> affichage(x) ^" - "^ affichage(y)
           |_ , Moins -> "( " ^ affichage(x) ^ " ) - " ^ affichage(y) 
           |Moins, _ -> affichage(x) ^ " - ( " ^ affichage(y) ^ " )"
           |_, _ ->   affichage(x) ^ " - " ^ affichage(y) 
         )
       |Node2(op1,_,_),_ -> if not( op1 = Moins)
                                 then " ( " ^ affichage(x) ^ " ) - " ^ affichage(y)
                                 else  affichage(x) ^ " - " ^ affichage(y)
       |_,Node2(op1,_,_) -> if not(op1 = Moins)
                             then   affichage(x) ^ " - ( " ^ affichage(y) ^ " )"
                             else  affichage(x) ^ " - " ^ affichage(y)
       |_ , _ ->   affichage(x) ^ " - " ^ affichage(y) 
     )
   |Node2(Div, x , y)  ->
     (
       match x , y with
       |Node2(op1,_,_) , Node2(op2,_,_) ->
         (
           match op1, op2 with
           |Div, Div -> affichage(x) ^ " / " ^affichage(y)
           |_ , Div -> "( " ^ affichage(x) ^ " ) / " ^ affichage(y) 
           |Div, _ -> affichage(x) ^ " / ( " ^ affichage(y) ^ ")"
           |_, _ ->  "( " ^ affichage(x) ^ " ) / ( " ^ affichage(y) ^ " )"
         )
       |Node2(op1 ,_,_),_ -> if not( op1 = Div)
                              then " ( " ^ affichage(x) ^ " ) / " ^ affichage(y)
                              else  affichage(x) ^ " / " ^ affichage(y)
       |_,Node2(op1,_,_) -> if not(op1 = Div)
                             then   affichage(x) ^ " / ( " ^ affichage(y) ^ " )"
                             else  affichage(x) ^ " / " ^ affichage(y)
       |_,_ ->  affichage(x) ^ " / " ^ affichage(y) )
   |Node2(Multiple, x , y)  ->
     (
       match x , y with
       |Node2(op1,_,_) , Node2(op2,_,_) ->
         (
           match op1, op2 with
           |Multiple, Multiple -> affichage(x) ^ " * " ^  affichage(y)
           |_ , Multiple -> "( " ^ affichage(x) ^ " ) * " ^ affichage(y) 
           |Multiple, _ -> affichage(x) ^ " * ( " ^ affichage(y) ^ " )"
           |_, _ ->  "( " ^ affichage(x) ^ " ) * ( " ^ affichage(y) ^ " )"
         )
       |Node2(op1 ,_,_),_ -> if not( op1 = Multiple)
                              then "( " ^ affichage(x) ^ " ) * " ^ affichage(y)
                              else  affichage(x) ^ " * " ^ affichage(y)
       |_,Node2(op1,_,_) -> if not(op1 = Multiple)
                             then   affichage(x) ^ " * ( " ^ affichage(y) ^ " )"
                             else  affichage(x) ^ " * " ^ affichage(y)
       |_,_ ->  affichage(x) ^ " * " ^ affichage(y)
     )
       
;;
