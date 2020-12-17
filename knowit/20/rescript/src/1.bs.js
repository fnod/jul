// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Int = require("bs-platform/lib/js/belt_Int.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Belt_SetInt = require("bs-platform/lib/js/belt_SetInt.js");

var nums = Belt_SetInt.fromArray(Belt_Array.range(1, 100000));

var input = Belt_SetInt.fromArray(Belt_Array.map(Belt_Array.map(Fs.readFileSync("../in/1.txt", "utf8").split(","), Belt_Int.fromString), Belt_Option.getExn));

console.log(Belt_Array.get(Belt_SetInt.toArray(Belt_SetInt.diff(nums, input)), 0));

exports.nums = nums;
exports.input = input;
/* nums Not a pure module */
