"use strict";

var assert = require('assert');

var Dice = function() {
  this.roll = function() {
    return (Math.random() * 6 + 1)|0;
  };

  return this;
};

describe('yahtzee', function() {
   describe('dice', function() {
     it('rolls a number between 1 and 6', function() {
       var dice = new Dice();
       
       var roll = dice.roll();
       assert(roll >= 1 && roll <= 6);       
     });

     it('stays the same until the next roll', function() {
       var dice = new Dice();

       var v = dice.roll();
       assert.equal(v, dice.value());
     });
   });

   describe('score', function() {
     it('exists', function() {
       assert(new Score(1,1,1,1,1));
     });
   });
});
