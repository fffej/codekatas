"use strict";

var assert = require('assert'); 

Array.prototype.binarySearch = function() {
  return -1;
};

describe('binary search', function() {
  it('doesnt find anything in an empty list', function() {
    assert.equal(-1, [].binarySearch(7));
  });
});
