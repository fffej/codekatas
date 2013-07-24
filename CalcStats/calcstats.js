"use strict";

var assert = require('assert');

var calcStats = function(data) {
  return { };
};

var testEmptyStats = function() {
  var stats = calcStats([]);
  assert.equal(undefined, stats.minValue);
  assert.equal(undefined, stats.maxValue);
  assert.equal(0, stats.count);
};

var test = function() {
  testEmptyStats();
};

test();
