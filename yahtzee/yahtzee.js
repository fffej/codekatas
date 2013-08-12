"use strict";

var assert = require('assert');

describe('yahtzee', function() {
   describe('dice', function() {
     it('rolls a number between 1 and 6', function() {
       var dice = new Dice();
       
       var roll = dice.roll();
       assert(roll >= 1 && roll <= 6);       
     });
   });
});
