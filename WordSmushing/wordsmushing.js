"use strict";

var assert = require('assert');

var smush2 = function(a,b) {
  if (b.indexOf(a) === 0) {
    return b;
  } else {
    return a[0] + smush2(a.substr(1), b);
  }
};

var nextArguments = function(args) {
  var x = args[0];
  var y = args[1];
 
  var rest = [smush2(x,y)];
  for (var i=2;i<args.length;++i) {
    rest.push(args[i]);
  }

  return rest;
}

var smush = function() {
  var n = arguments.length;
 
  if (n == 0) {
    return "";
  } else if (n == 1) {
    return arguments[0];
  } else {
    var rest = nextArguments(arguments);
    return smush.apply(null,rest);
  }
};

var score = function() {
  var sum = 0;
  for (var i=0;i<arguments.length;++i) {
     sum += arguments[i].length;
  }
  return sum;
};

describe('word smushing', function() {

  describe('scoring function', function() {
    it('is just length of args summed', function() {
      assert.equal(10, score('fives', 'a','duck'));
    });
  });

  describe('two work smushing', function() {
    it('should smush two equal words', function() {
      var smushed = smush2('to','to');
      assert.equal('to', smushed);
    });

    it('should smush two letter words', function() {
      var smushed = smush2('it', 'to');
      assert.equal('ito', smushed);
    });

    it('should be conjunction  when doesn\'t smush', function() {
      var unsmushed = smush2('be', 'at');
      assert.equal('beat', unsmushed);
    });
  });

  describe('three word smushing', function() {
    it('should work for minimal example', function() {
      var smushed = smush('minutes','testing', 'ginger');
      assert.equal('minutestinginger', smushed);
    });
  });

  describe('finds minimal smush', function() {
    it('works with simple example', function() {
      var smushed = minimalSmush('to', 'it');
      assert.equal('ito', smushed);
    });
  });
});
