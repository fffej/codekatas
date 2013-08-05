"use strict";

var On  = 1;
var Off = 0;

var Grid = function(w,h) {

  var grid = [];
  for (var i=0;i<h;++i) {
    grid[i] = [];
    for (var j=0;j<w;++j) {
      grid[i].push(Off);
    }
  }

  return {
    width:  function() { return w; },
    height: function() { return h; },
    state: function(x,y) { return grid[x][y]; },
    toggle: function(x,y) {
      if (this.state(x,y) === On) {
        grid[x][y] = Off;
      } else {
        grid[x][y] = On;
      }
    }
  };  
};

exports.Grid = Grid;
exports.On = On;
exports.Off = Off;
