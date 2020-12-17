let nums = Node.Fs.readFileSync("in/09.txt", #utf8)
-> Js.String2.trim
-> Js.String2.split("\n")
-> Belt.Array.map(Ut.pint)

let rec getInvalid = (ns: array<int>, i: int, pa: int) =>
    Belt.Array.slice(ns, ~offset=i-pa, ~len=pa)
    -> Ut.comba(2)
    //-> Belt.Array.some(([a, b]) => a+b == Belt.Array.getExn(ns, i)) WARN??
    -> Belt.Array.some(a => 
        Belt.Array.getExn(a, 0) + Belt.Array.getExn(a, 1)
        == Belt.Array.getExn(ns, i))
    ? getInvalid(ns, i+1, pa)
    : Belt.Array.getExn(ns, i)

let inv = nums->getInvalid(25, 25)
Js.log(inv)

let sum = (ns: array<int>) => Belt.Array.reduce(ns, 0, (acc,x) => acc + x)
let min = (ns: array<int>) => 
    Belt.Array.reduce(
        Belt.Array.sliceToEnd(ns, 1),
        Belt.Array.getExn(ns, 0),
        (mi, n) => n < mi ? n : mi)
let max = (ns: array<int>) => 
    Belt.Array.reduce(
        Belt.Array.sliceToEnd(ns, 1),
        Belt.Array.getExn(ns, 0),
        (ma, n) => n > ma ? n : ma)


let rec terms = (ns: array<int>, i: int, len: int, match: int) =>
    switch Belt.Array.slice(ns, ~offset=i, ~len=len) {
    | a when sum(a) == match => a
    | a when sum(a) < match => terms(ns, i, len+1, match)
    | a when sum(a) > match => terms(ns, i+1, 2, match)
    | _ => []
    }

let ts = nums -> terms(0, 2, inv)
(min(ts) + max(ts))->Js.log
