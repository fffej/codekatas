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

var toggle = function(door) {
  door.closed = !door.closed;
};

var visit = function(doors, step, visitor) {
  for(var i=0;i<doors.length;i++) {
    visitor(doors[i]);
  }
};


var main = function () {
  var doors = createDoors();
  var door = doorAt(doors,0);
  var count = 0;
 
  assert.equal(doorCount(doors), 100, "there should be 100 doors");
  assert(allDoorsAreClosed(doors), "all doors are closed");

  assert(isClosed(door), "door should be closed");
  toggle(door);
  assert(!isClosed(door), "door should be open");

  count = 0;
  var countingVisitor = function() { count++; };
  visit(doors,1,countingVisitor);
  assert.equal(count, 100, "100 doors visited");

  count = 0;
  visit(doors,51,countingVisitor);
  assert.equal(count, 1, "Single door visited");

};

main();