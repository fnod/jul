type command = (string, int, bool)
let pint = (s: string) => Belt.Int.fromString(s)->Belt.Option.getExn

let toCmd = (str: string): command =>
    switch Js.String2.match_(str, %re("/(\\w{3}) ([0-9+-]+)/")) {
    | Some([_, ins, val]) => (ins, pint(val), false)
    | _ => Js.Exn.raiseError(j`bad command: $str`) 
    }

let commands = Node.Fs.readFileSync("in/08.txt", #utf8)
-> Js.String2.trim
-> Js.String2.split("\n")
-> Belt.Array.map(toCmd)
-> Belt.Array.reduceWithIndex(Belt.Map.Int.empty, (a,c,i) =>
    Belt.Map.Int.set(a,i,c))

let rec run = (cs: Belt.Map.Int.t<command>, i: int, acc: int) =>
    i == Belt.Map.Int.size(cs)
    ? (true, acc)
    : switch Belt.Map.Int.getExn(cs, i) {
    | (_, _, true) => (false, acc)
    | ("acc", val, _) =>
        Belt.Map.Int.set(cs, i, ("acc", val, true)) 
        -> run(i+1, acc+val)
    | ("jmp", val, _) =>
        Belt.Map.Int.set(cs, i, ("jmp", val, true))
        -> run(i+val, acc)
    | ("nop", val, _) =>
        Belt.Map.Int.set(cs, i, ("nop", val, true))
        -> run(i+1, acc)
    | _ => Js.Exn.raiseError(j`invalid/no command at addr $i`)
    }
        
//part 1
commands
-> run(0, 0)
-> Js.log

//part 2
commands
-> Belt.Map.Int.keep((_, (ins, _, _)) => ins == "jmp" || ins == "nop")
-> Belt.Map.Int.toArray
-> Belt.Array.map( ((k,v)) =>
    switch v {
    | ("jmp", val, _) => Belt.Map.Int.set(commands, k, ("nop", val, false))
    | ("nop", val, _) => Belt.Map.Int.set(commands, k, ("jmp", val, false))
    | _ => Js.Exn.raiseError("unexpected instruction appeared")
    })
-> Belt.Array.map(p => run(p, 0, 0))
-> Belt.Array.getBy( ((terminated, _)) => terminated)
-> Js.log
