@bs.module("fs") external read: (string, string) => string = "readFileSync"

let re = %re("/(\d+)-(\d+) ([a-z]): ([a-z]+)/")
let parseInt = (str: string) => Belt.Int.fromString(str)->Belt.Option.getExn

let rec countInstances = (str: string, sub: string, c: int) =>
    switch Js.String.indexOf(sub, str) {
    | -1 => c
    |  i => countInstances(Js.String.sliceToEnd(~from=i+1, str), sub, c+1)
    }

let charPair = (str: string, i1: int, i2: int) =>
    (Js.String.charAt(i1, str), Js.String.charAt(i2, str))

let pwIsValid = (ps: array<string>) =>
    switch ps {
    | [_, min, max, pol, pw] =>
        switch countInstances(pw, pol, 0) {
        | n when n >= parseInt(min) && n <= parseInt(max) => true
        | _ => false
        }
    | _ => false
    }

let pwIsValid2 = (ps: array<string>) => 
    switch ps {
    | [_, i1, i2, pol, pw] =>
        switch charPair(pw, parseInt(i1)-1, parseInt(i2)-1) {
        | (c, d) when c == pol && d != pol => true
        | (c, d) when d == pol && c != pol => true
        | _ => false
        }
    | _ => false
    }

let pws = read("in/2.txt", "utf8")
-> Js.String.trim
-> Js.String2.splitByRe(%re("/\\r?\\n/")) 
-> Belt.Array.map(Belt.Option.getExn)
-> Belt.Array.map(x => Js.String2.match_(x, re))
-> Belt.Array.map(Belt.Option.getExn)

pws
-> Belt.Array.map(pwIsValid)
-> Belt.Array.keep(x => x)
-> Belt.Array.size
-> Js.log

pws
-> Belt.Array.map(pwIsValid2)
-> Belt.Array.keep(x => x)
-> Belt.Array.size
-> Js.log
