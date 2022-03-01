structure Babies =
struct
local
  open Csc330
in
  
(*
Student name: Jamie Sullivan-Phillips
Student id: V00865650

Add your code below this line

Remember: your submitted code cannot use print

*)


(* counts number of data entries in record *)
fun count_entries(l) =
  if null l
  then 0
  else 1 + count_entries(tl(l))

(* sums the values in the record *)
fun total(data : string list)=
  if null data
  then 0 
  else valOf(fromString(hd(data))) + total(tl data)
 

fun years(info : string list, n : int)=
  let
    val intel = tl(info)
    fun years_in(data : string list, i : int)=
      if i = n
      then 0
      else 
        if hd data <> "0" 
        then  1 + years_in(tl data, i + 1) 
        else years_in(tl data, i + 1)
  in
    years_in(intel, 1)
  end


fun last_year(data : string list, total : int, index : int)=
  let
    fun sum(data : string list)=
      if null data
      then 0
      else valOf(fromString(hd(data))) + sum( tl data )
   
    fun next_non_zero(list_of_years : string list, index : int)=
      if hd(list_of_years) <> "0"
      then (tl list_of_years, index)
      else next_non_zero(tl list_of_years, index + 1)
 
    val tuple = next_non_zero(data, index)
    val tail_list = #1 tuple
    val index = #2 tuple
  in
    if sum(tail_list) - total = 0 
    then index 
    else last_year(tail_list, total, index + 1)
  end

fun last_year_sum(data : string list, year : int)=
  let 
    fun last_year_sum_in(data : string list, index : int)=
      if index = year
      then valOf(fromString(hd data))
      else last_year_sum_in(tl data, index + 1)
  in
    last_year_sum_in(data, 0)
  end    

fun first_year(data : string list)=
  let
    fun first_year_in(data : string list, i : int)=
      if hd(data) <> "0"
      then (i, valOf(fromString(hd(data))))
      else first_year_in(tl data, i + 1)
  in
     first_year_in(data, 0)
  end

fun min(xs : string list) =
  if null xs
  then 0
  else if null (tl xs)
  then valOf(fromString(hd xs))
  else
    let 
      val tl_ans = min(tl xs)
      val hed = valOf(fromString(hd xs))
    in
      if hed < tl_ans andalso hed <> 0
      then hed
      else tl_ans
    end

fun min_year(data : string list, min : int)=
  if isSome(fromString(hd data))
  then
    let
      fun min_year_in(data: string list, i : int)=
        if valOf(fromString(hd data)) = min
        then i
        else min_year_in(tl data, i + 1)
    in
      min_year_in(data, 0)
    end
  else
    min_year(tl data, min)

fun max(data: string list, n: int)=
  let
    fun good_max(cur_max: int * int, l : string list, index : int) =
      if null l
      then cur_max
      else
        if valOf(fromString(hd l)) >= #1 cur_max 
        andalso index < n-1
        then good_max( (valOf(fromString(hd l)), index), tl l, index + 1)
        else good_max(cur_max, tl l, index + 1)
  in
    good_max((0,0), data, 0)
  end
  

fun avg(data : string list, n : int)=
  let
    fun sum(data : string list, i : int)=
      if i = n-1
      then int_to_real(0)
      else int_to_real(valOf(fromString(hd(data)))) + sum( tl data, i + 1 )
  in
    sum(tl data, 0) / int_to_real(n-1)
  end 

fun twenty19(data : string list, n : int)=
  let
    fun twenty19_in(data : string list, i : int)=
      if i = n-1
      then valOf(fromString(hd data))
      else twenty19_in(tl data, i + 1)
   in
     twenty19_in(data, 0)
   end
  


fun babies_program (babiesLines, NamesLines, offsetSt) =
(* the output of the program is the string returned by this function *)
  let
    val input's = split_at(NamesLines, #"\n")
    val record_entry's = split_at(babiesLines, #"\n")

    (* for each baby name in input call stats on that name *)
    fun find_record(name : string, record's : string list) = (*string list*)
      if null record's
      then []
      else 
        let
          fun name_check_records(record's : string list) =
            let
              val cur_record = hd(record's)
              val values = split_at(cur_record, #",")
              val r_name = hd(values)
            in
              if name = r_name then values else name_check_records(tl record's)
            end
         in
           name_check_records(record's) 
         end
         (*OK, now we can gather data for a name, now lets get stats*)
    fun get_stats(data : string list)=
      let
        val offset = valOf(fromString(offsetSt))
        val n = count_entries(data) - 1 
        val baby_name = hd(data)
        val total_num = total(tl data) div 2
        val years_num = years(data, n) 
        val last_year_num = last_year(tl data, total_num, 0)
        val last_year_sum_num = last_year_sum(data, last_year_num + 1)
        val first_num = first_year(tl data)
        val min_val = min(tl(data))
        val min_year_val = min_year(data, min_val) + offset
        val max_tuple = max(tl data, n)
        val avg_val = avg(data, n)
        val twenty19_val = twenty19(data, n)

        val record = {name = baby_name,
                     entries = int_to_string(n-1),
                     total = int_to_string(total_num),
                     years = int_to_string(years_num),
                     last_year = int_to_string(offset + last_year_num),
                     last_year_sum = int_to_string(last_year_sum_num),
                     first_year = int_to_string(#1 first_num + offset), 
                     first_year_val = int_to_string(#2 first_num),
                     min = int_to_string(min_val),
                     min_year = int_to_string(min_year_val),
                     max_year = int_to_string(#2 max_tuple + offset),
                     max_val = int_to_string(#1 max_tuple),
                     avg = real_to_string(avg_val),
                     twenty19_sum = int_to_string(twenty19_val)}
                           
      in
        record
      end
      (*returns true if there is a record for name, false otherwise *)
      fun record_exists(name : string, record's : string list)=
        if null record's
        then false
        else
          let
            val cur_record = hd(record's)
            val values = split_at(cur_record, #",")
            val r_name = hd(values)
          in
            if name = r_name then true else record_exists(name, tl record's)
          end

       (*for each val in input's, find record and get stats*)
       fun loop_names(name's : string list)=
         if null name's
         then ""
         else
           if record_exists(hd name's, record_entry's)
           then
             let
               val data = find_record(hd(name's), record_entry's) 
               val record = get_stats(data)
               val output = "\n" ^ #name record 
                  ^ "\n Total: " ^ #total record
                  ^ "\n Years: " ^ #years record
                  ^ "\n 2019: " ^ #twenty19_sum record
                  ^ "\n First: " ^ #first_year record ^ " " ^ #first_year_val record
                  ^ "\n Last: " ^ #last_year record ^ " " ^ #last_year_sum record
                  ^ "\n Min: " ^ #min_year record ^ " " ^ #min record
                  ^ "\n Max: " ^ #max_year record ^ " " ^  #max_val record
                  ^ "\n Avg: " ^ #avg record
             in
               output ^ loop_names(tl name's)
             end
           else
             "\n"^hd name's ^"\nBaby name ["^ hd name's ^ "] was not found" ^ loop_names(tl name's)
             
  in
    "Read 4340 babies. Starting year 1920. Each baby has 100 entries."
     ^ loop_names(input's)
     ^ "\n"
  end
  
end
end
    
