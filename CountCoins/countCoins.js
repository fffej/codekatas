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
  }
};

var testChange = function() {
  assert.equal(0, getChange(0));
  assert.equal(6, getChange(15));
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
