"use strict";

var assert = require('assert');

var calcStats = function(data) {
  return { 
    count: 0
  };
};

var testEmptyStats = function() {
  var stats = calcStats([]);
  assert.equal(undefined, stats.minValue);
  assert.equal(undefined, stats.maxValue);
  assert.equal(0, stats.count);
  assert.equal(undefined, stats.average);
};

var test = function() {
  testEmptyStats();
};

test();
