"use strict";

var Grid = function(w,h) {
  return {
    width:  function() { return w; },
    height: function() { return h; }
  };  
};

exports.Grid = Grid;
