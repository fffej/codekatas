"use strict";

var assert = require('assert');

var calcStats = function(data) {
  var min;
  for (var i=0;i<data.length;++i) {
    min = Math.min(data[i], min) || data[i];
  }

  return { 
    count: 0,
    minValue: min
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

  var zeroMin = calcStats([0]);
  assert.equal(0, zeroMin.minValue);
};

var test = function() {
  testEmptyStats();

  testMinimum();
};

test();
