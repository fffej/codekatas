"use strict";
var assert = require('assert');

var LIVE = 1;
var DEAD  = 0;

var cellState = function(currentCellState, numberOfAliveNeighbours) {
  var twoOrThreeNeighbours = numberOfAliveNeighbours === 2 ||
                             numberOfAliveNeighbours === 3;

  if (numberOfAliveNeighbours < 2 || numberOfAliveNeighbours > 3) {
    return DEAD;
  } 
  else if (currentCellState === LIVE && twoOrThreeNeighbours) { 
    return LIVE;
  }
  else if (currentCellState === DEAD && numberOfAliveNeighbours === 3) {
    return LIVE;
  } 
  else {
    return DEAD;
  }
};

var Grid = function(width, height) {
  var grid = [];
  for (var i=0;i<height;++i) {
    var row = [];
    for (var j=0;j<width;++j) {
      row.push(DEAD);
    }
    grid.push(row);
  }

  return {
    width: width,
    height: height,
    at: function(x,y) {
      assert(y < grid.length);
      assert(x < grid[0].length);
      return grid[x][y];
    },
    set: function(x,y,v) {
      assert(x < grid[0].length);
      assert(y < grid.length);
      console.log('grid: ' + grid.length);
      console.log('grid[x]:' + grid[x].length);
      console.log(grid[x][y]);
      grid[x][y] = v;
    }
  };
};

var createFromString = function(str) {
  var lines = str.split('\n');

  var hw = lines[0].split(' ');
  var h = hw[0] | 0;
  var w = hw[1] | 0;

  var grid = new Grid(w,h);

  console.log(w + "," + h);

  for (var i=0;i<h;++i) {
    var row = lines[i+1];
    assert.equal(w,row.length);
    for (var j=0;j<w;++j) {
      console.log(w + "," + h + "," + i + "," + j);
      grid.set(i,j,row[j] === '*' ? LIVE : DEAD);
    }
  }

  return grid;
};

describe('Game of life', function() {

  describe('grid', function() {
    it('should have a width and a height', function() {
      var grid = new Grid(10,20);
      assert.equal(10, grid.width);
      assert.equal(20, grid.height);
    });

    it('should be possible to read and write cells', function() {
      var grid = new Grid(10,20);

      assert.equal(DEAD, grid.at(0,0));

      grid.set(0,0, LIVE);
      assert.equal(LIVE, grid.at(0,0));

      grid.set(5,5, LIVE);
      assert.equal(LIVE, grid.at(5,5));
    });

    it('should be possible to read in a description of the grid', function() {
      var input = '4 8\n........\n....*...\n...**...\n........\n';
      var grid = createFromString(input);
      assert.equal(8, grid.width);
      assert.equal(4, grid.height);

      assert.equal(DEAD, grid.at(0,0));
      assert.equal(LIVE, grid.at(1,4));
      assert.equal(LIVE, grid.at(2,3));
      assert.equal(LIVE, grid.at(2,4));
    });
  });

  describe('cell', function() {
    it('fewer than two neighbours dies', function() {
      assert.equal(DEAD, cellState(LIVE,1));
    });

    it('more than 3 alive cells dies', function() {
      assert.equal(DEAD, cellState(LIVE,4));
    });

    it('live cells with 2 or 3 neighbours live on', function() {
      assert.equal(LIVE, cellState(LIVE,2));
      assert.equal(LIVE, cellState(LIVE,3));
    });

    it('dead cells with exactly 3 neighbours live', function() {
      assert.equal(LIVE, cellState(DEAD,3));
    });

    it('should be defined for all input types', function() {
      for (var i=0;i<=8;++i) {
        assert(undefined !== cellState(DEAD,i), 'dead with ' + i + 'neighbours');
        assert(undefined !== cellState(LIVE,i), 'live with ' + i + 'neighbours');
      }
    });
  });
});
