var assert = require('assert');

var getCoins = function() {
  return [1,5,10,25];
};

var possibleChangeFromSingleCoin = function(value,coins) {
  coins = coins || getCoins();

  var possibleChangeVals = [];
  for(var i=0;i<coins.length;++i) {
    if (coins[i] <= value) {
      possibleChangeVals.push(value - coins[i]);
    }
  }

  return possibleChangeVals;
};

var getChange = function(value,coins) {
  coins = coins || getCoins();

  var possibleChangeVals = possibleChangeFromSingleCoin(value,coins);
  var returnVals = [];
  for (var i=0;i<possibleChangeVals.length;++i) {
    var changeUsed = value - possibleChangeVals[i];
    var rest = getChange(possibleChangeVals[i],coins);
    
    if (rest.length === 0) {
      return [changeUsed];
    }
    else {
      for (var j=0;j<rest.length;++j) {
        returnVals.push([changeUsed].concat(rest[j]));
      }
    }
  }

  return sortAndUnique(returnVals);
};

var sortAndUnique = function(d) {
  var result = {};
  for (var i=0;i<d.length;++i) {
    d[i].sort();
    result[d[i]] = '';
  };

  var b = [];
  for(var k in result) {
    b.push(k.split(',').map(function(x) { return parseInt(x)}));
  };

  return b;
};

var testSortAndUnique = function () {
  assert.deepEqual([[1,2,3]], sortAndUnique([[1,2,3],[1,3,2]]));
  assert.deepEqual([[1,2,3],[1,2,4]], sortAndUnique([[1,2,3],[1,4,2]]));
};

var testChange = function() {
  assert.equal(0, getChange(0).length);
  assert.equal(1, getChange(1).length);
  assert.equal(2, getChange(5).length);
  assert.equal(6, getChange(15).length);
};

var testThereAreFourTypesOfCoins = function() {
  var coins = getCoins();
  assert.equal(coins.length, 4);
};

var testPossibleChange = function() {
  assert.equal(1, possibleChangeFromSingleCoin(1).length);

  assert.equal(4, possibleChangeFromSingleCoin(100).length);
};

var test = function() {
  testThereAreFourTypesOfCoins();
  testPossibleChange();
  testSortAndUnique();
  testChange();
};

test();
