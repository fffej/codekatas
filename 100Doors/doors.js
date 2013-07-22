var assert = require('assert');

var createDoors = function() {
  doors = [];
  for (var i=0;i<100;++i) {
    doors.push({ closed: true});
  }

  return doors;
};

var allDoorsAreClosed = function (doors) {
  var allDoorsClosed = true;
  for (var door in doors) {
    allDoorsClosed = allDoorsClosed && doors[door].closed;
  }

  return allDoorsClosed;
};

var doorCount = function(doors) {
  var doorCount = 0;
  for (var door in doors) {
    doorCount++;
  }

  return doorCount;
};

var isClosed = function(door) {
  return door.closed;
};

var doorAt = function(doors,index) {
  return doors[index];
};


var main = function () {
  var doors = createDoors();
 
  assert.equal(doorCount(doors), 100, "there should be 100 doors");
  assert(allDoorsAreClosed(doors), "all doors are closed");
  assert(isClosed(doorAt(doors,0)), "door 0 is closed");
};

main();