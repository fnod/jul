let init = (limit: int): array<int> =>
    Belt.Array.concat([2], Belt.Array.rangeBy(3, limit, ~step=2))

let is_prime = (n: int): bool =>
    !Belt.Array.some(
        init(n -> float -> sqrt -> Js.Math.floor_int),
        (x) => mod(n, x) == 0)

let rec nearest = (n: int): int => if is_prime(n) { n } else { nearest(n-1) }

let rec process = (num: int, count: int, lim: int) =>
    if num >= lim {
        count
    } else {
        if num -> Belt.Int.toString -> Js.String2.indexOf("7") >= 0 {
            process(num+nearest(num)+1, count, lim)
        } else {
            process(num+1, count+1, lim)
        }
    }

Js.log(process(0, 0, 5433000))
