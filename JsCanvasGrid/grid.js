// Daft contortion to avoid strict mode problems
"use strict";

var exportFn = function (grid,on,off,gridview) {
};

var On  = 1;
var Off = 0;

var GridView = function(grid) {

};
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

if (typeof(exports) !== 'undefined') {
  exports.Grid = Grid;
  exports.On = On;
  exports.Off = Off;
  exports.GridView = GridView;
}

