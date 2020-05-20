(*
#use "devoirf.ml" ;;
*)

type eb = V of int | TRUE | FALSE | AND of eb * eb | OR of eb * eb | XOR of eb * eb | NOT of eb | EQ of eb * eb ;;

(*fonction XOR car ocaml ne l'a pas déjà par defaut 
	comme "or" et "and"
	*)

let rec xor a b = match a,b with
	true,true -> false
	|true,false -> true
	|false,true -> true
	|false,false -> false
;;

let equa = EQ(XOR(V(1),V(2)),V(3)) ;;
let l = EQ(XOR(V(1),V(2)),FALSE)::EQ(V(2),V(2))::EQ(V(1),TRUE)::[]
let v = V(1)::V(2)::[] ;;

(* **************************** Question 1 ***************************** *)

(*  fonction identifieur qui prend en paramètre une expression de type eb
	ex : "EQ(XOR(V1,V2),TRUE)" pour identifier les variables qui la constitue
	dans notre exemple "[V1,V2]"
	*)


let rec identifieur x  = 
		match x with
		|V(n) -> V(n)::[]
		|TRUE|FALSE -> []
		|AND (x,y)|OR (x,y)|XOR (x,y)|EQ(x,y) -> (identifieur x)@(identifieur y)
		|NOT(n) -> identifieur n ;;
	
(* fonction ens_var qui prend en paramètre une liste d'équations et qui traite 
	les équations une à une pour constituer l'ensemble des variables 
	qui se trouvent dans toutes les équations
*)

let rec ens_var l = match l with
	|[]->[]
	|x::ll -> (identifieur x)@(ens_var ll) ;;

(* fonction appartient_var prend en paramètre un élement a et une liste l et renvoi 
	true si l'élement est dans la liste, false si non
 *)

let rec appartient_var a l = 
                    match l with
                    |[] -> false
                    |x::ll -> x = a || appartient_var a ll;;

(* fonction doublons_var qui prend en paramètre une liste l et qui supprime ses 
	doublons 
 *)

let rec doublons_var l =
                    match l with
                    |[] -> []
                    |x::ll -> if appartient_var x ll then doublons_var ll else x :: doublons_var ll;;

(*fonction nombre_var qui prend en paramètre une liste l qui renvoi son nombre de 
	variables *)
let rec nombre_var l = match l with 
	[]->0
	| x::ll -> 1 + nombre_var ll;;


(* ********************************* Question 2 *************************************)

(*Fonction map qui prend en parametre une fonction et permet d’aplliquer cet fonction sur chaque element d’une liste*)

let rec map f = function
  | [] -> []
  | a::l -> let r = f a in r :: map f l;;

(* fonction cherche qui prend en paramètre deux listes : l de valeurs de verités et x
	de variable et qui renvoi une liste de couples d'élements (variable,valeur de vérité)
	exp : "l = [TRUE,FALSE] et x = [V1;V2] on aura res = [(V1,TRUE);(V2,FALSE)]" 
*)

let rec cherche l x = match x,l with 
	[],[]->[]
	|x1::lx,y::ll -> (x1,y)::cherche ll lx
	;;

(* fonction ens_env qui prend en paramètre deux listes : l de toutes les valeurs de
	vérité possibles générées grâce à "ens_pos" et l1 la liste des variables, et qui renvoi 
	la liste de tous les environements possibles en utilisant la fonction "cherche" *)

let rec ens_env l l1 = match l with
	[]->[]
	|x::y -> cherche x l1@ens_env y l1;;
	
	
(* fonction ens_pos qui prend en parametre le nombre de variable qu’on possede dans notre systeme d’equation , on utilise la fonction concat qui concat le resultat de chaque possibilité dans une liste *)

let rec ens_pos n =
  if n = 0 then [[]]
  else
    let liste = ens_pos (n-1) in
    List.concat(map (fun aprec -> map(fun aprec2 -> aprec2@aprec)[[TRUE];[FALSE]]) liste);;

let a= ens_pos 2;;
let b= [V(1);V(2)];;




(* ******************************** Question 3 ****************************************)

(* fonction affect qui prend en paramètre une liste l de couples (Vn,valeur de vérité)
	et un entier n qui sert d'indice pour trouver notre variable et lui affecter sa valeur 
	de vérité
*)

let rec affect l n = 
	let a = List.nth l (n-1) in
	match a with 
		(x,y) -> match y with
				|TRUE -> true
				|FALSE -> false;;
				
(* fonction extract qui prend en paramètre une liste l des environnement possibles et deux entiers
	i comme indice de début et n comme indice de fin pour extraire les élements contenus entre les 
	deux indice  *)

(*let rec extract l lv n t= match l with
	[] -> lv
	|x::ll-> if n < (nombre_var l) then lv
			else x ::(extract ll lv (n*t) t)
	;;*)


let rec extract l i n =  if (n == i) then []
	else (List.nth l (i))::[]@(extract l (i+1) (n))
	
	;;

	
(* fonction verifie qui prend en paramètre eq une equation et lv une liste de couples 
	(Vn,valeur de vérité) qui renvoi "true" si les couples sont une solution pour 
	l'équation, "false" si non
*)

let rec verifie eq lv = match eq with
	V(n) -> affect lv n
	|TRUE -> true
	|FALSE -> false
	|NOT(x) -> not (verifie x lv)
	|OR(x,y) -> (verifie x lv) || (verifie y lv)
	|AND(x,y) -> (verifie x lv) && (verifie y lv)
	|XOR(x,y) -> xor (verifie x lv) (verifie y lv)
	|EQ(x,y) -> if ((verifie x lv) = (verifie y lv)) then true 
				else false 
;;

(*la fonction presolve qui prend en paramètre leq une liste d'equation, lv une liste
	de couples (Vn,valeur de vérité), lis_v une liste vide et n le nombre de variables (Vn)
	et qui évalue une à une les équation avec la liste des couples (Vn,valeur de vérité)
	 si toutes les équations sont vérifiées alors on renvoi la liste de couples (Vn,valeur de vérité)
	 si non on revoi une liste vide  *)

(*let rec presolve leq lv = match leq with
	[]-> []
	|x::y -> match x with
			 x-> (verifie x lv)::[]@presolve y lv@presolve ;;*)

let rec presolve leq lv lis_v n = match leq with 
	[]->[]
	|x::lleq -> if(((verifie x lv)=true)&& n != 1) then (presolve lleq lv lis_v (n-1))
	else if (((verifie x lv)=true)&& n = 1) then  lv::(presolve lleq lv lis_v (n-1))
	else [] ;;
		
let v1 = [EQ(V 1,V 2);EQ(OR(V 1,V 2),V 2)];;
let v2 = [(V(1),TRUE);(V(2),TRUE)];;

(* la fonction resolv qui prend en paramètre leq une liste d'équations, lv une liste de 
	couples (Vn,valeur de vérité), lis_v une liste vide, n le nombre de variables V(n),
	i indice de début "0" et lim le nombre d'élement de lv 
	permet de verifier toutes les possibilités
	grâce a extract qui extrait de la liste des environement possible, ni permettant 	de prendre le bonne intervalle lors de l’extractoin 
	une sous liste composée d'une des possibilité exp "[(V 1,TRUE);(V 2,FALSE)]"
	et presolve qui prend cette dernière avec la liste des equations et qui vérifie si 	cette 
	sous liste vérifie l'equation si oui on la stoque dans lis_v et on rappelle la 	fonction 
	récurssivement, si non on la fonction récurssivement sans stoquer la sous liste 
	*)

let rec resolv leq lv lis_v n neq i lim ni = if ((ni) <= lim) then match lv with 
	[]->lis_v
	|llv ->let x = (extract llv i ni ) in 
	 	(presolve leq x lis_v neq)@(resolv leq llv lis_v (n) neq (i+n) (lim) (ni+n))
	else [];;
(*fonction final qui prend en parametre le systeme d'equation et le resouds grace a l'appel de toute les fonctions pour donner la liste des couples qui satisfont le systeme d'equation*)
let rec final eq = let variable = doublons_var (ens_var eq) in
				   let nb_v = nombre_var(variable) in
				   let p = ens_pos (nb_v) in
				   let env = ens_env p v in 
				   resolv eq env [] nb_v (nombre_var eq) 0 (nombre_var env) nb_v;;
				   
	

