"use strict";

var assert = require('assert');
var toc = require('./toc.js');

describe('theory of constraints', function() {
  it('has a dice that rolls between 1 and 6', function() {
    for (var i=0;i<100;++i) {
      var roll = toc.rollDice();
      assert(roll >= 1 && roll <= 6);
    }
  });
});
