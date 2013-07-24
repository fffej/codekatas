"use strict";

var assert = require('assert');

var calcStats = function(data) {
  return {
    minValue: undefined
  };
};

var testEmptyStats = function() {
  var stats = calcStats([]);
  assert.equal(undefined, stats.minValue);
};

var test = function() {
  testEmptyStats();
};

test();
