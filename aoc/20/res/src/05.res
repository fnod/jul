let rec findPos = (s: string, l: int, u: int) =>
    switch Js.String2.length(s) {
    | 0 => -1
    | 1 =>
        switch Js.String2.get(s, 0) {
        | "F" | "L" => l
        | "B" | "R" => u
        | _ => -1
        }
    | _ => 
        switch Js.String2.get(s, 0) {
        | "F" | "L" => findPos(Js.String2.sliceToEnd(s, ~from=1), l, (l+u)/2)
        | "B" | "R" => findPos(Js.String2.sliceToEnd(s, ~from=1), (l+u)/2+1, u)
        | _ => -1
        }
    }

let findCol = (s: string) => findPos(s, 0, 7)
let findRow = (s: string) => findPos(s, 0, 127)

let seatID = (s: string) =>
    8 * findRow(Js.String2.slice(s, ~from=0, ~to_=7)) 
    + findCol(Js.String2.sliceToEnd(s, ~from=7))

let rec findGap = (l: list<int>) =>
    switch l {
    | list{h, ...t} when h - Belt.List.headExn(t) == 2 => h-1
    | list{_, ...t}  => findGap(t)
    | _ => -1
    }

let seats = Node.Fs.readFileSync("in/5.txt", #utf8)
-> Js.String2.trim
-> Js.String2.split("\n")
-> Belt.Array.map(seatID)
-> Js.Array2.sortInPlaceWith((a, b) => b - a)

let max = seats
-> Belt.Array.getExn(0)
Js.log(j`part 1: $max`)

let gap = seats
-> Belt.List.fromArray
-> findGap
Js.log(j`part 2: $gap`)
