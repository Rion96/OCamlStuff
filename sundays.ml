(* Any similarities to a Project Euler exercise are purely coincidental. ;) 
 * (Though for the record, I know that this is a terrible implementation. Just use Integers.)
 *)
module Date = struct
  type weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  type month = January | February | March | April | May | June | July | August | September | October | November | December
  type date = {
    weekday : weekday;
    day : int;
    month : month;
    year : int;
  }
  
  let make_date weekday day month year = {
    weekday = weekday;
    day = day;
    month = month;
    year = year;
  }

  let next_day date = (
    let week w = (
      match w with
      | Monday -> Tuesday
      | Tuesday -> Wednesday
      | Wednesday -> Thursday
      | Thursday -> Friday
      | Friday -> Saturday
      | Saturday -> Sunday
      | Sunday -> Monday
    ) in
    let day d = (
      let max_day = (
        match date.month with
        | September
        | April
        | June
        | November -> (
          30
        )
        | February -> (
          if (date.year mod 4 = 0 && date.year mod 100 <> 0)
          || (date.year mod 100 = 0 && date.year mod 400 = 0) then
            29
          else
            28
        )
        | _ -> (
          31
        )
      ) in
      if (d < max_day) then
        d + 1
      else
        1
    ) in
    let month m = (
      if day date.day = 1 then (
        match m with
        | January -> February
        | February -> March
        | March -> April
        | April -> May
        | May -> June
        | June -> July
        | July -> August
        | August -> September
        | September -> October
        | October -> November
        | November -> December
        | December -> January
      ) else (
        m
      )
    ) in
    let year y = (
      if (day date.day) = 1 && (month date.month = January) then
        y + 1
      else
        y
    ) in {
      weekday = (week date.weekday);
      day = (day date.day);
      month = (month date.month);
      year = (year date.year);
    }
  )

end

open Date

let _ = (
  let rec iter date counter = (
    if date.year < 2001 then (
      let next = next_day date in
      if next.year >= 1901 && next.weekday = Sunday && next.day = 1 then (
        iter next (counter + 1)
      ) else (
        iter next (counter)
      )
    ) else (
      counter
    )
  ) in
  iter (make_date Monday 1 January 1900) 0
  |> Printf.printf "%d\n"
)