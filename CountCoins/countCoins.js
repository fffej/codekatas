var assert = require('assert');

var makeChange = function() {
  return 1;
};

var test = function() {
  assert.equal(1, makeChange(1));
};

test();
