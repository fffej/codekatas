var assert = require('assert');

var testEmptyStats = function() {
  var stats = calcStats([]);
  assert.equal(undefined, stats.minValue);
};

var test = function() {
  testEmptyStats();
};

test();
