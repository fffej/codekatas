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

  describe('production system stage', function() {
    it('has a name', function() {
      var stage = toc.createStage('Analysis');
      assert.equal('Analysis', stage.name);
    });

    it('initially is empty', function() {
      var stage = toc.createStage('banana');
      assert.equal(0, stage.itemCount);
    });
 
    it('can receive items', function() {
      var stage = toc.createStage('banana');
      stage.receive(5);

      assert.equal(5, stage.itemCount);
    });

    it('can produce items', function() {
      var stage = toc.createStage('banana');
      stage.receive(5);

      var produced = stage.produce(1);
      assert.equal(4, stage.itemCount);
      assert.equal(1, produced);
    });

    it('can only produce as many items as it\'s capacity', function() {
      var stage = toc.createStage('banana');
      stage.receive(5);

      var produced = stage.produce(6);
      assert.equal(5, produced);
      assert.equal(0, stage.itemCount);
    });
  });
});
