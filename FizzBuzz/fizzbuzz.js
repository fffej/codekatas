"use strict";
var assert = require('assert');

var fizz = function(v) {
  return v % 3 === 0;
};

var buzz = function(v) {
  return v % 5 === 0;
};

describe("fizzbuzz", function() {
  it("should fizz", function() {
    assert(fizz(3)); 
    assert(!fizz(5));   
  });

  it("should buzz", function() {
    assert(!buzz(7));
    assert(buzz(10));
  });
});