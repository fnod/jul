let matrix = Node.Fs.readFileSync("in/3_matrix.txt", #utf8)
-> Js.String2.trim
-> Js.String2.split("\n")
-> Belt.Array.map(Js.String.split(""))

let rec seekWord = (
    word: string, cx: int, cy: int, dx: int, dy: int, m: array<array<string>>
) =>
    switch (Js.String2.get(word, 0), Js.String2.sliceToEnd(word, ~from=1)) {
    | (h, "") =>
        switch Belt.Array.get(m, cy) {
        | Some(y) =>
            switch Belt.Array.get(y, cx) {
            | Some(s) => h == s
            | None => false
            }
        | None => false
        }
    | (h, t) =>
        switch Belt.Array.get(m, cy) {
        | Some(y) =>
            switch Belt.Array.get(y, cx) {
            | Some(s) when h == s => seekWord(t, cx+dx, cy+dy, dx, dy, m)
            | _ => false
            }
        | None => false
        }
    }

let isPresent = (word: string, matrix: array<array<string>>) =>
    switch Js.String2.get(word, 0) {
    | c =>
        Belt.Array.mapWithIndex(matrix, (i, a) =>
            Belt.Array.mapWithIndex(a, (j, v) => c == c ? (j,i) : (-1,-1)))
        -> //flatten and filter out (-1, -1)

let words = Node.Fs.readFileSync("in/3_words.txt", #utf8)
-> Js.String2.trim
-> Js.String2.split("\n")
