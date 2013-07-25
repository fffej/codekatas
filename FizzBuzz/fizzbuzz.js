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
  if (fizzbuzz(v)) {
    return 'FizzBuzz';
  } else if (fizz(v)) {
    return 'Fizz';
  } else if (buzz(v)) {
    return 'Buzz';
  } else {
    return v + '';
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
    assert.equal('Fizz', strFizzBuzz(3));
    assert.equal('1', strFizzBuzz(1));
    assert.equal('Buzz', strFizzBuzz(5));
    assert.equal('FizzBuzz', strFizzBuzz(15));
  });

  it("runs correctly", function() {
    for (var i=1;i<=100;++i) {
      console.log(strFizzBuzz(i));
    }
  });
});

// Retro
// - Felt like this clicked more test-driven, but no concrete learnings other than that...