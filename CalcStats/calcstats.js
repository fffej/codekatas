"use strict";

var assert = require('assert');

var fold = function(data, initialValue, func) {
  var x = initialValue;
  for (var i=0;i<data.length;++i) {
    x = func(x,data[i]);
  }

  return x;
};

var calcStats = function(data) {

  var min = data.length === 0 ? undefined : fold(data,data[0], Math.min);
  var max = data.length === 0 ? undefined : fold(data,data[0], Math.max);

  return { 
    count: 0,
    minValue: min,
    maxValue: max
  };
};

var testEmptyStats = function() {
  var stats = calcStats([]);
  assert.equal(0, stats.count);
  assert.equal(undefined, stats.average);
};

var testMaximum = function() {
  var emptyList = calcStats([]);
  assert.equal(undefined, emptyList.maxValue);

  var singletonList = calcStats([1]);
  assert.equal(1, singletonList.maxValue);
};

var testMinimum = function() {
  var emptyList = calcStats([]);
  assert.equal(undefined, emptyList.minValue);

  var singletonList = calcStats([1]);
  assert.equal(1, singletonList.minValue);

  var zeroMin = calcStats([0]);
  assert.equal(0, zeroMin.minValue);

  var multiple = calcStats([0,1,2,3,4,5]);
  assert.equal(0, multiple.minValue);
};

var test = function() {
  testEmptyStats();
  testMinimum();
  testMaximum();
};

test();
