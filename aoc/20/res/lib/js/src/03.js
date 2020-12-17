// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");

var map = Belt_List.fromArray(Belt_Array.map(Fs.readFileSync("in/3.txt", "utf8").trim().split(/\r?\n/), Belt_Option.getExn));

function slide(_map, step, _x, _count) {
  while(true) {
    var count = _count;
    var x = _x;
    var map = _map;
    if (!map) {
      return count;
    }
    var t = map.tl;
    var h = map.hd;
    var match = h.charAt(x);
    if (match === "#") {
      _count = count + 1 | 0;
      _x = Caml_int32.mod_(x + step | 0, h.length);
      _map = t;
      continue ;
    }
    _x = Caml_int32.mod_(x + step | 0, h.length);
    _map = t;
    continue ;
  };
}

console.log(slide(map, 3, 0, 0));

console.log(Math.imul(Math.imul(Math.imul(Math.imul(slide(map, 1, 0, 0), slide(map, 3, 0, 0)), slide(map, 5, 0, 0)), slide(map, 7, 0, 0)), slide(Belt_List.keepWithIndex(map, (function (_x, i) {
                    return i % 2 === 0;
                  })), 1, 0, 0)));

exports.map = map;
exports.slide = slide;
/* map Not a pure module */
