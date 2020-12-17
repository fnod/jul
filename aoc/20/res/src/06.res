let answers = Node.Fs.readFileSync("in/6.txt", #utf8)
-> Js.String2.split("\n\n")

// part 1
answers
-> Belt.Array.map(Js.String.replaceByRe(%re("/\\n/g"), ""))
-> Belt.Array.map(Js.String.split(""))
-> Belt.Array.map(Belt.Set.String.fromArray)
-> Belt.Array.map(Belt.Set.String.size)
-> Belt.Array.reduce(0, (a,b) => a+b)
-> Js.log

// part 2
let incrementKey = (m: Belt.Map.String.t<int>, k: string) =>
    Belt.Map.String.set(m, k, Belt.Map.String.getWithDefault(m, k, 0) + 1)

let rec _countLetters = (s: string, m: Belt.Map.String.t<int>) =>
    switch (Js.String2.get(s, 0), Js.String2.sliceToEnd(s, ~from=1)) {
    | (h, "") => incrementKey(m, h)
    | (h, t)  => _countLetters(t, incrementKey(m, h))
    }
let countLetters = (str: string) =>
    Js.String2.replaceByRe(str, %re("/[^a-z]/g"), "")
    -> _countLetters(Belt.Map.String.fromArray([]))

let answeredByAll = (str: string) =>
    Js.String2.replaceByRe(str, %re("/\\n/g"), "")
    -> countLetters
    -> Belt.Map.String.valuesToArray
    -> Belt.Array.keep(n => 
        n == Js.String2.splitByRe(str, %re("/\\n/g")) -> Belt.Array.size)
    -> Belt.Array.size

answers
-> Belt.Array.map(answeredByAll)
-> Belt.Array.reduce(0, (a,b) => a+b)
-> Js.log
