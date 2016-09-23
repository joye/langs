(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s: string, sl: string list)=
  let fun all_except_str(s, sl, sr)=
	  case sl of
	      [] => (false, sr)
	    | x::xs =>
	      if same_string(s, x)
	      then (true, sr @ xs)
	      else all_except_str(s, xs, sr @ [x])
  in
      case all_except_str(s, sl, []) of
	  (false, _) => NONE
	| (true, x) => SOME x
  end
      
				     
fun get_substitutions1(sll: string list list, s: string) =
  case sll of
      [] => []
    | x::xs =>  let val sor = all_except_option(s, x)
		in
		    case sor of 
			NONE => get_substitutions1(xs, s)
		      | SOME x =>  x @ get_substitutions1(xs, s)
		end
		    
		    				
fun get_substitutions2(sll: string list list, s: string) = 
    let fun subs(sll, result) = 
	    case sll of
		[] => result
	      | x::xs => let val sor = all_except_option(s, x)
			 in
			     case sor of
				 NONE => subs(xs, result)
			      |  SOME z => subs(xs, result @ z)
			 end
    in
	subs(sll, [])
    end

fun similar_names (sll : string list list, {first = x, middle = y, last = z}) =
  let fun result_li (sl: string list, result)=
	case sl of
	    [] => result
	  | pn :: pnx => result_li(pnx, result @ [{first = pn, middle = y, last = z}])
  in
      result_li (get_substitutions2(sll, x), [{first = x, middle = y, last = z}])
  end
      										   	    
							 
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color card =
  case card of
      (Clubs, _) => Black
    | (Spades, _) => Black
    | _ => Red 

fun card_value card = 
    case card of
	(_, Num x) => x
      | (_, Ace) => 11
      | _ => 10

fun remove_card (cs, c, e) =
  let fun all_except_card (sl, sr) =
	case sl of
	    [] => (false, sr)
	  | x::xs => if c = x
		     then (true, sr @ xs)
		     else
			 all_except_card(xs, sr @ [x])
  in
      case all_except_card(cs, []) of
	  (false, _) => raise e
	| (true, x) => x
  end
      
fun all_same_color(cs) =
  case cs of
      [] => true
    | _::[] => true
    | head::(neck::rest) => card_color (head) = card_color(neck) andalso all_same_color(neck::rest) 

fun sum_cards cs =
  let fun sum_local (cs, result) =
	case cs of
	    [] => result
	  | x::xs => sum_local(xs, result + card_value(x))
  in
      sum_local(cs, 0)
  end

fun score (cs, goal) =
  let val sum_card = sum_cards cs
      val color = all_same_color cs
      val preli = if sum_card > goal then 3*(sum_card-goal) else goal-sum_card
  in
      if color = true then preli div 2 else preli
  end
      
							    
fun officiate (cardl, movel, goal)=
  let fun state(next_cardl, next_movl, result_list) =
	case next_movl of
	    [] => result_list
	  | (Discard x) :: xs => state(next_cardl, xs, remove_card(result_list, x, IllegalMove))
	  | Draw :: xs => case next_cardl of
			      [] => result_list
			    | y::ys => if sum_cards(result_list @ [y]) > goal then result_list @ [y]
				       else
					   state(ys, xs, result_list @[y])
  in
      score (state(cardl, movel, []), goal)
  end	   
				      
