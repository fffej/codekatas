var assert = require('assert');

var getCoins = function() {
  return [1,5,10,25];
};

var testThereAreFourTypesOfCoins = function() {
  var coins = getCoins();
  assert.equal(coins.length, 4);
};

var test = function() {
  testThereAreFourTypesOfCoins();
};

test();
