var assert = require('assert');

var allDoorsAreClosed = function () {
  var allDoorsClosed = true;
  for (var door in doors) {
    allDoorsClosed = allDoorsClosed && door.closed;
  }
};


var main = function () {
  assert(allDoorsAreClosed(), "all doors are closed");
};

main();