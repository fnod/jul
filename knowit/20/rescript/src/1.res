@bs.module("fs") external read: (string, string) => string = "readFileSync"

let nums = Belt.Array.range(1, 100_000)
  -> Belt.Set.Int.fromArray
let input = read("../in/1.txt", "utf8")
  -> Js.String2.split(",")
  -> Belt.Array.map(Belt.Int.fromString)
  -> Belt.Array.map(Belt.Option.getExn)
  -> Belt.Set.Int.fromArray

Belt.Set.Int.diff(nums, input)
  ->Belt.Set.Int.toArray
  ->Belt.Array.get(0)
  ->Js.log
