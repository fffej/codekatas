"use strict";

var On  = 1;
var Off = 0;

var Grid = function(w,h) {
  return {
    width:  function() { return w; },
    height: function() { return h; }
  };  
};

exports.Grid = Grid;
exports.On = On;
exports.Off = Off;
