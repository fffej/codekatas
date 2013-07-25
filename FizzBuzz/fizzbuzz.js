"use strict";
var assert = require('assert');

var fizz = function(v) {
  return v % 3 === 0;
};

var buzz = function(v) {
  return v % 5 === 0;
};

var fizzbuzz = function(v) {
  return fizz(v) && buzz(v);
};

var strFizzBuzz = function(v) {
  if (fizz(v)) {
    return 'fizz';
  }
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

  it("should fizz buzz", function() {
    assert(fizzbuzz(15));
  });

  it("should return correct string", function() {
    assert.equal('fizz', strFizzBuzz(3));
  });
});