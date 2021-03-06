// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Int = require("bs-platform/lib/js/belt_Int.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");

var kre = /(.+?) bags contain/;

var vre = /(\d) (.+?) bags?[,.]/g;

function pint(str) {
  return Belt_Option.getExn(Belt_Int.fromString(str));
}

function unwrapNullable(nu) {
  return Belt_Option.getExn((nu == null) ? undefined : Caml_option.some(nu));
}

function rscn(re, str, _matches) {
  while(true) {
    var matches = _matches;
    var r = re.exec(str);
    if (r === null) {
      return matches;
    }
    _matches = Belt_Array.concat(matches, [Belt_Array.sliceToEnd(r, 1)]);
    continue ;
  };
}

function regexScan(re, str) {
  return rscn(re, str, []);
}

function shinyGold(color, tree) {
  var a = Belt_MapString.get(tree, color);
  if (a !== undefined) {
    if (Belt_Array.some(a, (function (x) {
              return Belt_Array.getExn(x, 1) === "shiny gold";
            }))) {
      return true;
    } else {
      return Belt_Array.some(a, (function (x) {
                    return shinyGold(Belt_Array.getExn(x, 1), tree);
                  }));
    }
  } else {
    return false;
  }
}

function cost(color, tree) {
  var ar = Belt_MapString.get(tree, color);
  if (ar !== undefined) {
    return Belt_Array.reduce(Belt_Array.map(ar, (function (a) {
                      var str = Belt_Array.getExn(a, 0);
                      var str$1 = Belt_Array.getExn(a, 0);
                      return Belt_Option.getExn(Belt_Int.fromString(str)) + Math.imul(Belt_Option.getExn(Belt_Int.fromString(str$1)), cost(Belt_Array.getExn(a, 1), tree)) | 0;
                    })), 0, (function (acc, x) {
                  return acc + x | 0;
                }));
  } else {
    return 0;
  }
}

var tree = Belt_Array.reduce(Fs.readFileSync("in/07.txt", "utf8").trim().split("\n"), undefined, (function (acc, x) {
        var cs = regexScan(vre, x);
        if (cs.length !== 0) {
          return Belt_Array.reduce(cs, acc, (function (a, c) {
                        var k = Belt_Array.getExn(Belt_Option.getExn(Caml_option.null_to_opt(x.match(kre))), 1);
                        return Belt_MapString.set(a, k, Belt_Array.concat(Belt_MapString.getWithDefault(a, k, []), [Belt_Array.map(c, unwrapNullable)]));
                      }));
        } else {
          return acc;
        }
      }));

console.log(Belt_Array.keep(Belt_MapString.keysToArray(tree), (function (x) {
            return shinyGold(x, tree);
          })).length);

console.log(cost("shiny gold", tree));

exports.kre = kre;
exports.vre = vre;
exports.pint = pint;
exports.unwrapNullable = unwrapNullable;
exports.rscn = rscn;
exports.regexScan = regexScan;
exports.shinyGold = shinyGold;
exports.cost = cost;
exports.tree = tree;
/* tree Not a pure module */
