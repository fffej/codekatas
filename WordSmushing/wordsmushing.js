"use strict";

var Strategy = {
  BruteForce: function() {}
};

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

var selectMinimumAccordingToScoreFn = function(xs) {
  return xs.reduce(function(x,y) {
      return score(y) < score(x) ? y : x;
    },xs[0]);
};

var allFutureArguments = function(args) {
  var allPossibilities = [];
  var n = args.length;
  for (var i=0;i<n;++i) {
    for (var j=0;j<n;++j) {
      if (j == i) {
        continue;
      }
      var x = args[i];
      var y = args[j];

      var rest = [smush(x,y)];
      for (var k=0;k<n;++k) {
        if (k !== i && k !== j) {
          rest.push(args[k]);
        }
      }
      allPossibilities.push(rest);
    }
  }
  return allPossibilities;
};

var minimalSmush = function() {

  // Drop the first argument (which is a strategy)
  var n = arguments.length - 1;
  var selection = arguments[0];
  var args = [];
  for (var i=1;i<arguments.length;++i) {
    args.push(arguments[i]);
  }

  if (n == 0) {
    return ;
  } else if (n == 1) {
    return args[0];
  } else {
    
    var allPossibilities = selection(allFutureArguments(args));

    var everything = allPossibilities.map(function(args) {
      return smush.apply(null, args);
    });
    
    return everything.reduce(function(x,y) {
      return score(y) < score(x) ? y : x;
    },everything[0]);
  }
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
    it('uses the strategy', function() {
      var used = false;
      var strategy = function(x) { 
        used = true;
        return x;
      };

      var smushed = minimalSmush(strategy,'to', 'it');
      assert(used);
    });

    it('works with simple example', function() {
      var smushed = minimalSmush(Strategy.BruteForce,'to', 'it');
      assert.equal('ito', smushed);
    });

    it('works with a longer example', function() {
      var smushed = minimalSmush(
        Strategy.BruteForce,
        'testing', 
        'ginger', 
        'german', 
        'minutes'
        );

      assert.equal('minutestingingerman', smushed);
    });
  });
});
