var assert = require('assert');

var testThereAreFourTypesOfCoins = function() {
  var coins = getCoins();
  assert.equal(coins.length, 4);
};

var test = function() {
  testThereAreFourTypesOfCoins();
};

test();
