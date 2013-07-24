var assert = require('assert');

var getCoins = function() {
  return [1,5,10,25];
};

var testThereAreFourTypesOfCoins = function() {
  var coins = getCoins();
  assert.equal(coins.length, 4);
};

var testPossibleChange = function() {
  assert.equal(1, possibleChange(1).length);
};

var test = function() {
  testThereAreFourTypesOfCoins();
  testPossibleChange();
};

test();
