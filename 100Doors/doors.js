var assert = require('assert');

var createDoors = function() {
  doors = [];
  for (var i=0;i<100;++i) {
    doors.push({});
  }
};

var doors = createDoors();

var allDoorsAreClosed = function () {
  var allDoorsClosed = true;
  for (var door in doors) {
    allDoorsClosed = allDoorsClosed && door.closed;
  }

  return allDoorsClosed;
};

var thereAre100Doors = function() {
  var doorCount = 0;
  for (var door in doors) {
    doorCount++;
  }

  return doorCount;
};


var main = function () {
  assert(thereAre100Doors(), "there should be 100 doors");
  assert(allDoorsAreClosed(), "all doors are closed");
};

main();