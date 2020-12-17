@bs.module("fs") external read: (string, string) => string = "readFileSync"

let prsi = (s: string) => s->Belt.Int.fromString->Belt.Option.getExn

let isValidPassport = (m) =>
    switch Belt.Map.String.keysToArray(m) {
    | a when Belt.Array.size(a) == 8 => true 
    | a when Belt.Array.size(a) == 7
      && !Belt.Array.some(a, x => x=="cid") => true
    | _ => false
    }

let validYear = (byr: string, min: int, max: int) =>
    switch Belt.Int.fromString(byr) {
    | Some(y) when y >= min && y <= max => true
    | _ => false
    }

let validHeight = (hgt: string) =>
    switch Js.String2.match_(hgt, %re("/(\d{2,3})(in|cm)/")) {
    | Some([_,v,u]) when u == "cm" && v->prsi >= 150 && v->prsi <= 193 => true
    | Some([_,v,u]) when u == "in" && v->prsi >= 59 && v->prsi <= 76 => true
    | _ => false
    }

let validHcl = (hcl: string) =>
    switch Js.String2.match_(hcl, %re("/^#[0-9a-f]{6}$/")) {
    | Some(_) => true
    | None => false
    }

let validEcl = (ecl: string) =>
    ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    -> Belt.Array.some(c => c == ecl)

let validPid = (pid: string) =>
    switch Js.String2.match_(pid, %re("/^\d{9}$/")) {
    | Some(_) => true
    | None => false
    }

let passports = read("in/4.txt", "utf8")
-> Js.String.trim
-> Js.String2.split("\n\n") 
-> Belt.Array.map(s => Js.String2.replaceByRe(s, %re("/\\n/g"), " "))
-> Belt.Array.map(s => 
    Js.String2.split(s, " ")
    -> Belt.Array.map(Js.String.split(":"))
    -> Belt.Array.map(kv => (kv[0], kv[1])))
-> Belt.Array.map(Belt.Map.String.fromArray)

let validPassports = passports
-> Belt.Array.keep(isValidPassport)
let nvp = Belt.Array.size(validPassports)
Js.log(j`part 1: $nvp`)

let withValidFields = validPassports
-> Belt.Array.keep(p =>
    Belt.Map.String.getExn(p, "byr")->validYear(1920, 2002)
    && Belt.Map.String.getExn(p, "iyr")->validYear(2010, 2020)
    && Belt.Map.String.getExn(p, "eyr")->validYear(2020, 2030)
    && Belt.Map.String.getExn(p, "hgt")->validHeight
    && Belt.Map.String.getExn(p, "hcl")->validHcl
    && Belt.Map.String.getExn(p, "ecl")->validEcl
    && Belt.Map.String.getExn(p, "pid")->validPid)
let nvf = Belt.Array.size(withValidFields)
Js.log(j`part 2: $nvf`)
