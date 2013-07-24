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

var testMinimum = function() {
  var emptyList = calcStats([]);
  assert.equal(undefined, emptyList.minValue);

  var singletonList = calcStats([1]);
  assert.equal(1, singletonList.minValue);
};

var test = function() {
  testEmptyStats();

  testMinimum();
};

test();
