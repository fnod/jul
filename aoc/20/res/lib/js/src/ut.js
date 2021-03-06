// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Int = require("bs-platform/lib/js/belt_Int.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");

function pint(str) {
  return Belt_Option.getExn(Belt_Int.fromString(str));
}

function comb(l, m) {
  if (m === 0) {
    return {
            hd: /* [] */0,
            tl: /* [] */0
          };
  }
  if (!l) {
    return /* [] */0;
  }
  var t = l.tl;
  var h = l.hd;
  return Belt_List.concat(Belt_List.map(comb(t, m - 1 | 0), (function (ll) {
                    return Belt_List.concat({
                                hd: h,
                                tl: /* [] */0
                              }, ll);
                  })), comb(t, m));
}

function comba(l, m) {
  if (m === 0) {
    return [[]];
  }
  if (l.length === 0) {
    return [];
  }
  var h = Belt_Array.getExn(l, 0);
  var t = Belt_Array.sliceToEnd(l, 1);
  return Belt_Array.concat(Belt_Array.map(comba(t, m - 1 | 0), (function (x) {
                    return Belt_Array.concat([h], x);
                  })), comba(t, m));
}

exports.pint = pint;
exports.comb = comb;
exports.comba = comba;
/* No side effect */
