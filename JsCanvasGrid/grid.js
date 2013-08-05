// Daft contortion to avoid strict mode problems
var exportFn = function (grid,on,off,gridview) {
  exports.Grid = grid;
  exports.On = on;
  exports.Off = off;
  exports.GridView = gridview;
};

(function() {
  "use strict";
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

  exportFn(Grid,On,Off,GridView);  
}());

