type op = Plus|Moins| Multiple | Divise ;;

type uni =  Arite1;;


type tree = Cst of int
             |Var of string
             |Exp of string
             |Uni of uni * tree
             |Op of op * tree * tree
             |EMPTY   

let  is_cst(s : string) : bool=
  let result : bool ref = ref (s.[0] >= '0' && s.[0] <='9')  in

  for i = 0 to String.length(s)-1  do

    if s.[i] >= '0' && s.[i] <='9' then
      result := true 
    else result := false 
  done;
  !result
  
;;

is_cst("");;

let string_to_int(s : string) : int =
  
  for i = 0 to String.length(s)-1  do

    


;;
                    
let rec parse_aux (l : string list) (s :string list )(suiv : bool) : tree list  =

  match l, s with
  |[] , _ -> []
  |hd::[] , v::[] ->(match hd with
                       |"+" -> [Op(Plus, Exp(v) , EMPTY)]
                       |"-" -> [Op(Moins, Exp(v) , EMPTY)]
                       |"/" -> [Op(Divise, Exp(v) , EMPTY)]
                       |"*" -> [Op(Multiple,Exp(v) , EMPTY)]
                       |x -> failwith ""
                      )
  |hd::tail, v::[] -> (match hd with
                       |"+" -> Op(Plus, EMPTY , EMPTY)::parse_aux (tail) s true
                       |"-" -> Op(Moins, EMPTY , EMPTY)::parse_aux (tail) s true
                       |"/" -> Op(Divise, EMPTY , EMPTY)::parse_aux (tail) s true
                       |"*" -> Op(Multiple,EMPTY , EMPTY)::parse_aux (tail) s true
                       |x -> parse_aux (tail) (hd::s) false
                      )
  |hd::tail, v1::v2::[] -> (
                              match hd with
                              |"+" -> if suiv = false then  Op(Plus, Exp(v1) ,Exp(v2))::parse_aux (tail) [] true
                                      else Op(Plus, Exp(v1) ,EMPTY)::parse_aux (tail) (v2::[]) true
                              |"-" -> if suiv = false then  Op(Moins, Exp(v1) ,Exp(v2))::parse_aux (tail) [] true
                                      else Op(Moins, Exp(v1) ,EMPTY)::parse_aux (tail) (v2::[]) true
                              |"/" -> if suiv = false then  Op(Divise, Exp(v1) ,Exp(v2))::parse_aux (tail) [] true
                                      else Op(Divise, Exp(v1) ,EMPTY)::parse_aux (tail) (v2::[]) true
                              |"*" ->if suiv = false then  Op(Multiple, Exp(v1) ,Exp(v2))::parse_aux (tail) [] true
                                      else Op(Multiple, Exp(v1) ,EMPTY)::parse_aux (tail)(v2::[]) true
                              |x -> parse_aux (tail)( hd::s) false
  )
  |hd::tail , v1::v2::reste -> (match hd with
                                |"+" -> if suiv = false then  Op(Plus, Exp(v1) ,Exp(v2))::parse_aux (tail) reste true
                                      else Op(Plus, Exp(v1) ,EMPTY)::parse_aux (tail) (v2::reste) true
                                |"-" -> if suiv = false then  Op(Moins, Exp(v1) ,Exp(v2))::parse_aux (tail) reste true
                                      else Op(Moins, Exp(v1) ,EMPTY)::parse_aux (tail) (v2::reste) true
                                |"/" ->if suiv = false then  Op(Moins, Exp(v1) ,Exp(v2))::parse_aux (tail) reste true
                                      else Op(Divise, Exp(v1) ,EMPTY)::parse_aux (tail) (v2::reste) true
                                |"*" -> if suiv = false then  Op(Moins, Exp(v1) ,Exp(v2))::parse_aux (tail) reste true
                                      else Op(Multiple, Exp(v1) ,EMPTY)::parse_aux (tail) (v2::reste) true
                                |x -> parse_aux (tail)( hd::s) false
                               )
                                         
  |hd::tail, [] ->(match hd with
                   |"+" -> Op(Plus ,EMPTY, EMPTY)::parse_aux (tail) [] true
                    |"-" -> Op( Moins, EMPTY, EMPTY)::parse_aux (tail) [] true
                    |"/" -> Op(Divise, EMPTY ,EMPTY)::parse_aux (tail) [] true
                    |"*" -> Op(Multiple, EMPTY, EMPTY)::parse_aux (tail) [] true
                    |x -> parse_aux (tail)( [hd]@s) false
                     
                  )
;;


let rec parse_tree  (a : tree list) : tree  =

    match a with
    |[] -> EMPTY
    |hd::hd2::tail ->( match hd with
                  |Op( x , EMPTY, EMPTY) -> Op(x, hd2, parse_tree(tail))
                  |Op(x , y, EMPTY) -> Op(x, y , parse_tree(hd2::tail))
                  |Op( x, y, z) -> Op( x, y, z) )
    |hd::[] ->  ( match hd with
                  |Op( x, y, z) -> Op( x, y, z) )
                           
;;


let parse(list : string list) : tree =
  let tree : tree  = parse_tree(List.rev(parse_aux (list) [] false)) in

  tree;;


  
parse ["7";"14";"4";"2";"3";"+";"-";"*";"+"] ;;


parse(["13" ;"2"; "5";"*";"1";"0";"/";"-";"+"]) ;;







