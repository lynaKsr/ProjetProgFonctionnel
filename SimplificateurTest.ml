(*
        MEMBRES DU GROUPE :
                - KESSOURI Lyna
                - HO Nguyen Quynh Nhu
                - IRCA Jayson 
 *)

#use "ProjetProgFonct.ml"
   
(*************************** TEST FONCTION PARSE *******************************)

let testParse1() =
  let expression = ["x"] in
  let resultatAttendu = Var 'x' in
  let resultatDeLaFonct = parse expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;

let testParse2() =
  let expression = ["5"] in
  let resultatAttendu = Cst 5 in
  let resultatDeLaFonct = parse expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;

let testParse3() =
  let expression = ["3000"] in
  let resultatAttendu = Cst 3000 in
  let resultatDeLaFonct = parse expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;

let testParse4() =
  let expression = ["x";"4";"+"] in
  let resultatAttendu = Node2(Plus, Var 'x', Cst 4) in
  let resultatDeLaFonct = parse expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;

let testParse5() =
  let expression = ["13";"2";"~";"*"] in
  let resultatAttendu = Node2 (Multiple, Cst 13, Minus (Cst 2)) in
  let resultatDeLaFonct = parse expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;

let testParse6() =
  let expression = ["13";"2";"5";"*";"1";"0";"/";"-";"+"] in
  let resultatAttendu = Node2 (Plus, Cst 13,
 Node2 (Moins, Node2 (Multiple, Cst 2, Cst 5), Node2 (Div, Cst 1, Cst 0))) in
  let resultatDeLaFonct = parse expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;

testParse1() ;;
testParse2() ;;
testParse3() ;;
testParse4() ;;
testParse5() ;;
testParse6() ;;

(************************ TEST DE LA FOCNTION SIMPLIFICATION *************************)

let testSimplification1() =
  let expression = Cst(3) in
  let resultatAttendu = Cst 3 in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;

let testSimplification2() =
  let expression = Var('c') in
  let resultatAttendu =Var 'c' in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;

let testSimplification3() =
  let expression = Minus(Cst(5)) in
  let resultatAttendu = Minus (Cst 5) in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;

let testSimplification4() =
  let expression = Node2(Plus,Cst(4),Cst(6)) in
  let resultatAttendu = Cst 10 in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;
let testSimplification5() =
  let expression = Node2(Moins,Cst(4),Cst(10)) in
  let resultatAttendu = Minus (Cst 6) in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;
let testSimplification6() =
  let expression = Node2(Moins,Cst(12),Cst(10)) in
  let resultatAttendu = Cst 2 in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;
let testSimplification7() =
  let expression = (Node2(Multiple,Cst(4),Cst(5))) in
  let resultatAttendu = Cst 20 in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;
let testSimplification8() =
  let expression = Node2(Div,Cst(15),Cst(3)) in
  let resultatAttendu = Cst 5 in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;
let testSimplification9() =
  let expression = Node2(Div,Cst(4),Cst(3)) in
  let resultatAttendu = Cst 1 in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;
let testSimplification10() =
  let expression = Node2(Plus,Var('x'),Cst(0)) in
  let resultatAttendu = Var('x') in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;
let testSimplification11() =
  let expression = Node2(Plus,Cst(0),Var('x')) in
  let resultatAttendu = Var('x') in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;
let testSimplification12() =
  let expression = Node2(Plus,Var('x'),Var('y')) in
  let resultatAttendu = Node2 (Plus, Var 'x', Var 'y') in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;
let testSimplification13() =
  let expression = Node2(Multiple,Var('x'),Cst(0)) in
  let resultatAttendu = Cst 0 in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;
let testSimplification14() =
  let expression = Node2(Multiple,Cst(1),Var('x')) in
  let resultatAttendu = Var 'x' in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;

let testSimplification15() =
  let expression = Node2(Multiple,Var('x'),Cst(1)) in
  let resultatAttendu = Var 'x' in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;
let testSimplification16() =
  let expression = Node2(Moins,Node2(Plus,Var('x'),Var('y')),Var('y')) in
  let resultatAttendu = Node2(Moins, Node2(Plus, Var 'x', Var 'y'), Var 'y') in
  let resultatDeLaFonct = simplification expression in
  assert (resultatDeLaFonct = resultatAttendu)
;;

testSimplification1() ;;
testSimplification2() ;;
testSimplification3() ;;
testSimplification4() ;;
testSimplification5() ;;
testSimplification6() ;;
testSimplification7() ;;
testSimplification8() ;;
testSimplification9() ;;
testSimplification10() ;;
testSimplification11() ;;
testSimplification12() ;;
testSimplification13() ;;
testSimplification14() ;;
testSimplification15() ;;
testSimplification16() ;;

(******************************* TEST FONCTION AFFICHAGE *********************************)

let test_affichage1() =
  let expression = Cst 3 in
  let resultatAttendu = "3" in 
  let resultatDeLaFonct = affichage expression in
  assert(resultatAttendu = resultatDeLaFonct)
;;

let test_affichage2() =
  let expression = Var 'x' in
  let resultatAttendu = "x" in 
  let resultatDeLaFonct = affichage expression in
  assert(resultatAttendu = resultatDeLaFonct)
;;

let test_affichage3() =
  let expression = Minus(Cst 3) in
  let resultatAttendu = "( ~ 3 )" in 
  let resultatDeLaFonct = affichage expression in
  assert(resultatAttendu = resultatDeLaFonct)
;;

let test_affichage4() =
  let expression = Node2(Plus, Cst 10 , Var 'y') in
  let resultatAttendu = "10 + y" in 
  let resultatDeLaFonct = affichage expression in
  assert(resultatAttendu = resultatDeLaFonct)
;;

let test_affichage5() =
  let expression = Node2(Multiple,Node2(Plus, Cst 10 , Var 'y'), Cst 8) in
  let resultatAttendu = "( 10 + y ) * 8" in 
  let resultatDeLaFonct = affichage expression in
  assert(resultatAttendu = resultatDeLaFonct)
;;

let test_affichage6() =
  let expression = Node2(Plus,Node2(Plus, Cst 10 , Var 'y'), Cst 8) in
  let resultatAttendu = "10 + y + 8" in 
  let resultatDeLaFonct = affichage expression in
  assert(resultatAttendu = resultatDeLaFonct)
;;

let test_affichage7() =
  let expression = Node2(Multiple, Node2(Multiple , Node2(Div , Cst 8 , Var 'z'), Cst 15), Node2(Multiple, Var 'y', Var 't')) in
  let resultatAttendu = "( 8 / z ) * 15 * y * t" in 
  let resultatDeLaFonct = affichage expression in

  assert(resultatAttendu = resultatDeLaFonct)
;;

let test_affichage8() =
  let expression = Node2(Plus, Node2(Moins , Node2(Multiple , Cst 2 , Cst 5), Node2(Div , Cst 1 , Cst 0)), Cst 15 ) in  
  let resultatAttendu = "( 2 * 5 - 1 / 0 ) + 15" in 
  let resultatDeLaFonct = affichage expression in
  assert(resultatAttendu = resultatDeLaFonct)
;;

let test_affichage9() =
  let expression = Node2(Multiple, Node2(Multiple , Var 'a' , Node2(Multiple, Var 'b' , Var 'c')), Node2(Plus , Var 'e' , Var 'f')) in  
  let resultatAttendu = "a * b * c * ( e + f )" in 
  let resultatDeLaFonct = affichage expression in
  assert(resultatAttendu = resultatDeLaFonct)
;;



test_affichage1();;
test_affichage2();;
test_affichage3();;
test_affichage4();;
test_affichage5();;
test_affichage6();;
test_affichage7();;
test_affichage8();;
test_affichage9();;



