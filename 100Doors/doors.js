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
  for(var i=step + -1;i<doors.length;i+=step) {
    visitor(doors[i]);
  }
};

var kata = function(doors) {
  for (var i=1;i<=100;++i) {
    visit(doors,i,toggle);
  }
};

var test = function () {
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

  assert.equal(doors[51].closed, true, "door closed after visitor");
  visit(doors,52,toggle);
  assert.equal(doors[51].closed, false, "doors opened after visitor");
};

var kata = function () {
  var doors = createDoors();
  var closedDoors = [];
  var openDoors = [];

  for (var i=0;i<100;++i) {
    visit(doors,i+1,toggle);
  }

  for (var i=0;i<100;++i) {
    if (isClosed(doorAt(doors,i))) {
      closedDoors.push(i+1);
    } else {
      openDoors.push(i+1);
    }
  }

  console.log('Open doors: ' + openDoors);
  console.log('Closed doors: ' + closedDoors);
};

test();

kata();