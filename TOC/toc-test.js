"use strict";

var assert = require('assert');
var toc = require('./toc.js');

describe('theory of constraints', function() {
  it('has a dice that rolls between 1 and 6', function() {
    var nums = [0,0,0,0,0,0];
    for (var i=0;i<100;++i) {
      var roll = toc.rollDice();
      assert(roll >= 1 && roll <= 6);
      nums[roll-1]++;
    }

    for (var i=0;i<nums.length;++i) {
      assert(nums[i] > 0);
    }
  });

  describe('production system', function() {
    it('stage has a name', function() {
      var stage = toc.createStage('Analysis');

      assert.equal('Analysis', stage.name);
    });
  });
});
