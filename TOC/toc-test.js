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
    it('is created from stages', function() {
      var system = toc.createSystem(toc.createStage('Pot'));

      assert(system);
    });

    it('items are fed into the first stage', function() {
      var stage = toc.createStage('Pot');
      var system = toc.createSystem(stage);
      system.feed(10);

      assert.equal(10, stage.itemCount);
    });
  });

  describe('production system stage', function() {

    var stage = null;

    beforeEach(function() {
      stage = toc.createStage('Analysis');
    });

    it('has a name', function() {
      assert.equal('Analysis', stage.name);
    });

    it('initially is empty', function() {
      assert.equal(0, stage.itemCount);
    });
 
    it('can receive items', function() {
      stage.receive(5);
      assert.equal(5, stage.itemCount);
    });

    it('can produce items', function() {

      stage.receive(5);

      var produced = stage.produce(1);
      assert.equal(4, stage.itemCount);
      assert.equal(1, produced);
    });

    it('can only produce as many items as it\'s capacity', function() {
      stage.receive(5);

      var produced = stage.produce(6);
      assert.equal(5, produced);
      assert.equal(0, stage.itemCount);
    });
  });
});
