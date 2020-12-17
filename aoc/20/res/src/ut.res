let pint = (str: string) => Belt.Int.fromString(str)->Belt.Option.getExn

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

let rec comba = (l: array<int>, m: int): array<array<int>> =>
    switch (l, m) {
    | (_, 0) => [[]]
    | ([], _) => []
    | (a, n) => {
        let (h, t) = (Belt.Array.getExn(a, 0), Belt.Array.sliceToEnd(a, 1))
        Belt.Array.concat(
            comba(t, n-1)->Belt.Array.map(x => Belt.Array.concat([h], x)),
            comba(t, n)
        )
    }}
