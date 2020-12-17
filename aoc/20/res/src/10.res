type counter = { d1: int, d3: int, prev: int }

let nums = Node.Fs.readFileSync("in/10.txt", #utf8)
-> Js.String2.trim
-> Js.String2.split("\n")
-> Belt.Array.map(Ut.pint)
-> Belt.List.fromArray
-> Belt.List.sort((a, b) => a - b)

let diffies = nums
-> Belt.List.reduce({d1: 0, d3: 1, prev: 0}, (acc, n) =>
    switch n - acc.prev {
    | 1 => {...acc, d1: acc.d1+1, prev: n}
    | 3 => {...acc, d3: acc.d3+1, prev: n}
    | _ => acc
    })

Js.log(diffies.d1 * diffies.d3)

let terminus = Belt.List.headExn(nums->Belt.List.reverse)

let rec forks = (l: list<int>, term: int) =>
    switch l {
    | list{_, ...t} when t == list{term} => 1
    | list{_, ...t} when t == list{} => 0
    | list{h, ...t} => {
        let next = Belt.List.keep(t, x => x-h < 4)
        //Js.log(j`$h, ` ++ Js.String.make(next->Belt.List.toArray))
        next -> Belt.List.mapWithIndex((i, _) =>
            forks(Belt.List.drop(t, i)->Belt.Option.getExn, term))
        -> Belt.List.reduce(0, (acc, y) => acc + y)
        }
    | _ => 0
    }

[16,10,15,5,1,11,7,19,6,12,4]
-> Belt.List.fromArray
-> Belt.List.sort((a,b) => a - b)
-> forks(16)
-> Js.log

[28,33,18,42,31,14,46,20,48,47,24,23,49,45,
19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]
-> Belt.List.fromArray
-> Belt.List.sort((a,b) => a - b)
-> forks(48)
-> Js.log
//forks(nums, terminus)
//-> Js.log
