var assert = require('assert');

var doors = [];

var allDoorsAreClosed = function () {
  var allDoorsClosed = true;
  for (var door in doors) {
    allDoorsClosed = allDoorsClosed && door.closed;
  }

  return allDoorsClosed;
};


var main = function () {
  assert(allDoorsAreClosed(), "all doors are closed");
};

main();