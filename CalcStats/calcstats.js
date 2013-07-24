"use strict";

var assert = require('assert');

var calcStats = function(data) {

  var listIsOfLength0 = data.length === 0;
 
  var min = listIsOfLength0 ? undefined : data.reduce(function(x,y) { return Math.min(x,y); });
  var max = listIsOfLength0 ? undefined : data.reduce(function(x,y) { return Math.max(x,y); });
  var cnt = listIsOfLength0 ? 0 : data.reduce(function(x,y) { return  x + 1; }, 0);
  var sum = listIsOfLength0 ? 0 : data.reduce(function(x,y) { return x + y; });

  return { 
    average: listIsOfLength0 ? undefined : (sum / cnt),
    count: cnt,
    minValue: min,
    maxValue: max
  };
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

var testNumberOfElements = function() {
  assert.equal(0, calcStats([]).count);
  assert.equal(1, calcStats([1]).count);
  assert.equal(5, calcStats([5,6,7,8,9]).count);
};

var testAverage = function() {
  var stats = calcStats([]);
  assert.equal(undefined, stats.average);

  var zero = calcStats([0]);
  assert.equal(0, zero.average);

  var three = calcStats([0,1,2]);
  assert.equal(1, three.average);
};

var test = function() {
  testMinimum();
  testMaximum();
  testNumberOfElements();
  testAverage();

  console.log(calcStats([6,9,15,-2,92,11]));
};

test();

// Retrospectives
// - using fold is nice
// - you can use .reduce
// - but reduce is weird, takes 4 arguments (not 2)
// - this does make multiple passes through the list, but that's
// easily fixed
