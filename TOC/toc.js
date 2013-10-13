"use strict";

var rollDice = function() {
  return Math.random() * 6 + 1 | 0;
}

var createStage = function(name) {
  return new Stage(name);
};

var Stage = function(name) {

  this.name = name;
  this.itemCount = 0;
  this.receive = function(unitCount) {
    this.itemCount += unitCount;
  };

  this.produce = function(unitCount) {
    this.itemCount -= unitCount;
    return unitCount;
  };

  return this;
};

if (typeof('')) {
  exports.rollDice = rollDice;
  exports.createStage = createStage;  
}
