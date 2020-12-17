let parseInt = (str: string) => Belt.Int.fromString(str)->Belt.Option.getExn

let rec comb = (l: list<int>, m: int): list<list<int>> =>
    switch (l, m) {
    | (_, 0) => list{list{}}
    | (list{}, _) => list{}
    | (list{h, ...t}, n) =>
        Belt.List.concat(
            comb(t, n-1)->Belt.List.map(ll => Belt.List.concat(list{h}, ll)),
            comb(t, n)
        )
}

let expenses = Node.Fs.readFileSync("in/01.txt", #utf8)
-> Js.String2.trim
-> Js.String2.split("\n")
-> Belt.Array.map(parseInt)
-> Belt.List.fromArray

expenses
-> comb(2)
-> Belt.List.getBy(p => Belt.List.reduce(p, 0, (a,i) => a+i) == 2020)
-> Belt.Option.getExn
-> Belt.List.reduce(1, (a,i) => a*i)
-> Js.log

expenses
-> comb(3)
-> Belt.List.getBy(p => Belt.List.reduce(p, 0, (a,i) => a+i) == 2020)
-> Belt.Option.getExn
-> Belt.List.reduce(1, (a,i) => a*i)
-> Js.log
