(* Union Find Data Structure *)
module UnionFind = struct
  type uf = {
    parent : int array;
    size : int array;
  }
  exception NotInUnionFind
  let make_uf size = (
    if size <= 0 then (
      raise (Invalid_argument "Size has to be greater than 0!")
    );
    let ret = {
      parent = Array.make size 0;
      size = Array.make size 1;
    } in
    for i = 0 to (size - 1) do
      ret.parent.(i) <- i
    done;
    ret
  )
  let find uf elem = (
    if (elem >= Array.length uf.parent) then (
      raise NotInUnionFind
    );
    (* Find root *)
    let rec iter input = (
      if (uf.parent.(input) = input) then (
        input
      ) else (
        iter uf.parent.(input)
      )
    ) in
    let root = iter elem in
    (* Compress path *)
    let rec iter input = (
      if input != root then (
        let next_elem = uf.parent.(input) in
        uf.parent.(input) <- root;
        iter next_elem
      )
    ) in
    iter elem;
    root
  )
  let union uf a b = (
    let a, b = find uf a, find uf b in
    if (a <> b) then (
      (* Weighted Union: Update smaller component *)
      let a_size, b_size = uf.size.(a), uf.size.(b) in
      if (a_size < b_size) then (
        uf.parent.(a) <- b;
        uf.size.(b) <- a_size + b_size
      ) else (
        uf.parent.(b) <- a;
        uf.size.(a) <- a_size + b_size
      )
    )
  )
end

open Scanf
open Printf
open UnionFind

let _ = (
  let t = scanf " %d " (fun x -> x) in
  for i = 1 to t do
    let n, m = scanf " %d %d " (fun n m -> n, m) in
    let uf = make_uf (n + 1) in
    let enemy = Array.make (n + 1) 0 in
    let update_hate a b = (
      if enemy.(a) > 0 then (
        union uf enemy.(a) b
      );
      if enemy.(b) > 0 then (
        union uf a enemy.(b)
      );
      enemy.(a) <- b;
      enemy.(b) <- a
    ) in
    for j = 1 to m do
      let c, a, b = scanf " %c %d %d " (fun c a b -> c, find uf a, find uf b) in
      if c = 'F' then (
        if enemy.(a) > 0 && enemy.(b) > 0 then (
          union uf enemy.(a) enemy.(b);
          enemy.(a) <- 0;
          enemy.(b) <- 0
        );
        union uf a b
      ) else (
        update_hate a b
      )
    done;
    printf "Case #%d: %s\n" i (if uf.size.(find uf 1) * 2 > n then "yes" else "no")
  done
)