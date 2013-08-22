"use strict";

var assert = require('assert'); 

describe('binary search', function() {
  it('doesnt find anything in an empty list', function() {
    assert.equal(-1, [].binarySearch(7));
  });
});
