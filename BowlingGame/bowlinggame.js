"use strict";

var assert = require('assert');

var bowl = function(hits) {
  return 300;
};

var test = function() {
  assert.equal(2, bowl(1,1));
};

test();