// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");

function findPos(_s, _l, _u) {
  while(true) {
    var u = _u;
    var l = _l;
    var s = _s;
    var match = s.length;
    if (match === 0) {
      return -1;
    }
    if (match !== 1) {
      var match$1 = s[0];
      var exit = 0;
      switch (match$1) {
        case "F" :
        case "L" :
            exit = 1;
            break;
        case "B" :
        case "R" :
            exit = 2;
            break;
        default:
          return -1;
      }
      switch (exit) {
        case 1 :
            _u = (l + u | 0) / 2 | 0;
            _s = s.slice(1);
            continue ;
        case 2 :
            _l = ((l + u | 0) / 2 | 0) + 1 | 0;
            _s = s.slice(1);
            continue ;
        
      }
    } else {
      var match$2 = s[0];
      switch (match$2) {
        case "F" :
        case "L" :
            return l;
        case "B" :
        case "R" :
            return u;
        default:
          return -1;
      }
    }
  };
}

function findCol(s) {
  return findPos(s, 0, 7);
}

function findRow(s) {
  return findPos(s, 0, 127);
}

function seatID(s) {
  return (findPos(s.slice(0, 7), 0, 127) << 3) + findPos(s.slice(7), 0, 7) | 0;
}

function findGap(_l) {
  while(true) {
    var l = _l;
    if (!l) {
      return -1;
    }
    var t = l.tl;
    var h = l.hd;
    if ((h - Belt_List.headExn(t) | 0) === 2) {
      return h - 1 | 0;
    }
    _l = t;
    continue ;
  };
}

var seats = Belt_Array.map(Fs.readFileSync("in/5.txt", "utf8").trim().split("\n"), seatID).sort(function (a, b) {
      return b - a | 0;
    });

var max = Belt_Array.getExn(seats, 0);

console.log("part 1: " + max);

var gap = findGap(Belt_List.fromArray(seats));

console.log("part 2: " + gap);

exports.findPos = findPos;
exports.findCol = findCol;
exports.findRow = findRow;
exports.seatID = seatID;
exports.findGap = findGap;
exports.seats = seats;
exports.max = max;
exports.gap = gap;
/* seats Not a pure module */
