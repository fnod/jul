let kre = %re("/(.+?) bags contain/")
let vre = %re("/(\\d) (.+?) bags?[,.]/g")
type bush = Belt.Map.String.t<array<array<string>>>

let pint = (str: string) => Belt.Int.fromString(str)->Belt.Option.getExn

let unwrapNullable = (nu: Js.nullable<string>) =>
    nu->Js.Nullable.toOption->Belt.Option.getExn

let rec rscn = (re: Js.Re.t, str: string, matches) =>
    switch Js.Re.exec_(re, str) {
    | Some(r) => 
        rscn(re, str,
            Belt.Array.concat(matches, 
                [Belt.Array.sliceToEnd(Js.Re.captures(r), 1)]
            ))
    | None => matches
    }
let regexScan = (re: Js.Re.t, str: string) => rscn(re, str, [])

let rec shinyGold = (color: string, tree: bush) =>
    switch Belt.Map.String.get(tree, color) {
    | Some(a) =>
        Belt.Array.some(a, x => Belt.Array.getExn(x,1) == "shiny gold")
        ? true
        : Belt.Array.some(a, x => shinyGold(Belt.Array.getExn(x,1), tree))
    | None => false
    }

let rec cost = (color: string, tree: bush) =>
    switch Belt.Map.String.get(tree, color) {
    | None => 0
    | Some(ar) => 
        Belt.Array.map(ar, a =>
            pint(Belt.Array.getExn(a,0)) + pint(Belt.Array.getExn(a,0))
            * cost(Belt.Array.getExn(a,1), tree))
        -> Belt.Array.reduce(0, (acc, x) => acc+x)
    }

let tree = Node.Fs.readFileSync("in/07.txt", #utf8)
-> Js.String2.trim
-> Js.String2.split("\n")
-> Belt.Array.reduce(Belt.Map.String.empty, (acc,x) =>
    switch regexScan(vre, x) {
    | [] => acc
    | cs => Belt.Array.reduce(cs, acc, (a, c) => {
        let k = Js.String2.match_(x, kre)
            -> Belt.Option.getExn
            -> Belt.Array.getExn(1)
        Belt.Map.String.set(
            a,
            k,
            Belt.Map.String.getWithDefault(a, k, [])
                -> Belt.Array.concat(
                    [c->Belt.Array.map(unwrapNullable)])
        )})
    })

Belt.Map.String.keysToArray(tree)
-> Belt.Array.keep(x => shinyGold(x, tree))
-> Belt.Array.size
-> Js.log

cost("shiny gold", tree)->Js.log
