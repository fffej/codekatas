"use strict";

var assert = require('assert');

var bowl = function(hits) {

};

var test = function() {
  var score = bowl(10,10,10,10,10,10,10,10,10,10);
  assert.equal(300,score);

};

test();