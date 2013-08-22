"use strict";

var assert = require('assert'); 

Array.prototype.binarySearch = function(n) {

  var O = Object(this);

  var f = function(lo,hi) {
    var mid = Math.floor((lo + hi) / 2);
    if (O[mid] === n) {
      return mid;
    } 

    if(O[mid] < n) {
      return f(mid,hi);
    }
    return -1;
  };

  return f(0, O.length);
};

describe('binary search', function() {
  it('doesnt find anything in an empty list', function() {
    assert.equal(-1, [].binarySearch(7));
  });

  it('finds a single element', function() {
    assert.equal(0, [0].binarySearch(0));
  });

  it('finds the middle element', function() {
    assert.equal(1, [1,2,3].binarySearch(2));
  });

  it('finds up', function() {
    assert.equal(2, [1,2,3].binarySearch(3));
  });
});
