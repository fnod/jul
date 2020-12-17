@bs.module("fs") external read: (string, string) => string = "readFileSync"

let map = read("in/3.txt", "utf8")
-> Js.String.trim
-> Js.String2.splitByRe(%re("/\\r?\\n/")) 
-> Belt.Array.map(Belt.Option.getExn)
-> Belt.List.fromArray

let rec slide = (map: list<string>, step: int, x: int, count: int) =>
    switch map {
    | list{} => count
    | list{h, ...t} =>
        switch Js.String.charAt(x, h) {
        | "#" => slide(t, step, mod(x+step, Js.String.length(h)), count+1)
        | _ => slide(t, step, mod(x+step, Js.String.length(h)), count)
        }
    }

map->slide(3, 0, 0)->Js.log

( map->slide(1, 0, 0)
* map->slide(3, 0, 0)
* map->slide(5, 0, 0)
* map->slide(7, 0, 0)
* map->Belt.List.keepWithIndex((_x, i) => mod(i, 2) == 0)->slide(1, 0, 0)
)->Js.log
