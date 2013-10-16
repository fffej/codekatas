"use strict";

var rollDice = function() {
  return Math.random() * 6 + 1 | 0;
}

var createSystem = function() {
  var stages = [];
  for (var i=0;i<arguments.length;++i) {
    stages.push(arguments[i]);
  }

  return new System(stages);
};

var System = function(stages) {

   this.stages = stages;

   this.feed = function(materials) {
     this.stages[0].receive(materials);
   };

   this.tick = function(timeSourceFn) {
     var stageCount = this.stages.length;

     var workItems = this.stages[0].produce(timeSourceFn())
     for (var i=1;i<stageCount;++i) {
       this.stages[i].receive(workItems);
       
       if (i !== stageCount - 1) {
         workItems = this.stages[i].produce(timeSourceFn());
       }
     }
   };

   return this;
};

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
    unitCount = Math.min(this.itemCount,unitCount);
    this.itemCount -= unitCount;
    return unitCount;
  };

  return this;
};

if (typeof('')) {
  exports.rollDice = rollDice;
  exports.createStage = createStage;  
  exports.createSystem = createSystem;
}
