// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");

var matrix = Belt_Array.map(Fs.readFileSync("in/3_matrix.txt", "utf8").trim().split("\n"), (function (param) {
        return param.split("");
      }));

function seekWord(_word, _cx, _cy, dx, dy, m) {
  while(true) {
    var cy = _cy;
    var cx = _cx;
    var word = _word;
    var match = word[0];
    var match$1 = word.slice(1);
    if (match$1 === "") {
      var y = Belt_Array.get(m, cy);
      if (y === undefined) {
        return false;
      }
      var s = Belt_Array.get(y, cx);
      if (s !== undefined) {
        return match === s;
      } else {
        return false;
      }
    }
    var y$1 = Belt_Array.get(m, cy);
    if (y$1 === undefined) {
      return false;
    }
    var s$1 = Belt_Array.get(y$1, cx);
    if (s$1 === undefined) {
      return false;
    }
    if (match !== s$1) {
      return false;
    }
    _cy = cy + dy | 0;
    _cx = cx + dx | 0;
    _word = match$1;
    continue ;
  };
}

var words = Fs.readFileSync("in/3_words.txt", "utf8").trim().split("\n");

exports.matrix = matrix;
exports.seekWord = seekWord;
exports.words = words;
/* matrix Not a pure module */