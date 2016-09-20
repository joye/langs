fun is_older(a : int*int*int, b: int*int*int) =
  if #1 a < #1 b
  then true
  else
      if #1 a = #1 b
      then
	  if #2 a < #2 b
	  then true
	  else
	      if #2 a = #2 b
	      then
		  if #3 a < #3 b
		  then true
		  else
		      false
	      else
		  false
      else
	  false

fun number_in_month(xs: (int*int*int) list, month: int) =
  if null xs
  then 0
  else
      let val tl_number = number_in_month((tl xs), month)
      in
	  if #2 (hd xs) = month
	  then 1 + tl_number
	  else tl_number
      end
	  
fun number_in_months(xs: (int*int*int) list, months: int list) =
  if null months
  then 0
  else
      number_in_month(xs, (hd months)) + number_in_months(xs, (tl months))

fun dates_in_month(xs : (int*int*int) list, month: int) =
  if null xs
  then []
  else
      let val tl_list = dates_in_month((tl xs), month)
      in
          if #2 (hd xs) = month then
	      (hd xs) :: tl_list
	  else
	      tl_list
      end
fun dates_in_months(xs:(int*int*int) list, months: int list) =
  if null months
  then []
  else
      dates_in_month(xs, (hd months)) @ dates_in_months(xs, (tl months))

fun get_nth(s: string list, n:int) =
  if n = 1
  then (hd s)
  else
      get_nth((tl s), n-1)
      
fun date_to_string(xs:int*int*int)=
  let val ys=["January","February","March","April","May","June","July","August","September", "October","November","December"]
  in
      get_nth(ys, #2 xs) ^ " " ^ Int.toString(#3 xs) ^ ", " ^ Int.toString(#1 xs)
  end

fun number_before_reaching_sum(sum: int, a : int list) =
  if null a
  then 0
  else
      if hd(a) < sum
      then 1+number_before_reaching_sum(sum-hd(a), tl(a))
      else 0

fun what_month(day: int) =
  let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
     1 + number_before_reaching_sum(day, months)
  end

fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else
      what_month(day1)::month_range(day1+1, day2)

fun oldest(xs: (int*int*int) list)=
  if null xs
  then NONE
  else
      let
	  fun oldest_nonempty (xs: (int*int*int) list) =
	     if null (tl xs)
	     then hd xs
	     else
	          let val tl_ans = oldest_nonempty(tl xs)
	          in
		     if is_older(hd(xs), tl_ans)
		     then hd(xs)
		     else tl_ans
	      end
      in
	  SOME (oldest_nonempty xs)
      end
	  
      
	   
           

      
