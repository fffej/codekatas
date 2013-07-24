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

  if (value === 0) {
    return [];
  } else {
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
    return returnVals;
  }
};

var testChange = function() {
  assert.equal(0, getChange(0).length);
  assert.equal(1, getChange(1).length);
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
  testChange();
};

test();
