let bresenham f t = (
  let print_triple t = (
    let x, y, p = t in
    Printf.printf "%d\t%d\t%d\n" x y p
  ) in
  let x1, y1 = f in
  let x2, y2 = t in
  let dx, dy = x2 - x1, y2 - y1 in
  let p = 2 * dy - dx in
  let rec loop currx curry p = (
    if currx <= x2 then (
      print_triple (currx, curry, p);
      if p < 0 then (
        let p = p + 2 * dy in
        loop (currx + 1) curry p
      ) else (
        let p = p + 2 * dy - 2 * dx in
        loop (currx + 1) (curry + 1) p
      )
    )
  ) in
  loop x1 y1 p
)