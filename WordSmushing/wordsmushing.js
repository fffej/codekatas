"use strict";

var assert = require('assert');

var canSmush = function(a,b) {
  if (a === '') { return false; }

  if (b.indexOf(a) === 0) {
    return true;
  } else {
    return canSmush(a.substr(1), b);
  }
};

var smushIndex = function(a,b) {
  if (!canSmush(a,b)) throw Error('unsmushable:' + a + ":" + b);

  if (b.indexOf(a) === 0) {
    return 0;
  } else {
    return 1 + smushIndex(a.substr(1),b);
  }
};

var smush = function(a,b) {
  if (!canSmush(a,b)) throw Error('unsmushable');
  return a.substr(0,smushIndex(a,b)) + b;
};

var tryReduceArguments = function(args,i,j) {
  if (canSmush(args[i], args[j])) {
    var newArgs = [];
    newArgs.push(smush(args[i],args[j]));

    for (var k=0;k<args.length;++k) {
      if (k !== i && k !== j) {
        newArgs.push(args[k]);
      }
    }
    return newArgs;
  } else {
    return undefined;
  }
};

var multiSmush = function() {

  if (arguments.length === 1) {
    return arguments[0];
  } else {
    var n = arguments.length;

    var nextSetOfArguments = [];

    for (var i=0;i<n;++i) {
      for (var j=0;j<n;++j) {
        if (i === j) {
          continue;
        }
           
        var posArgs = tryReduceArguments(arguments,i,j);
        if (posArgs) {
          nextSetOfArguments.push(posArgs);
        }
      }
    }

    // Now I've got every set of possible arguments
    if (nextSetOfArguments.length === 0) {
      throw new Error('simply doesnt smush');
    }

    var results = [];
    for (var i=0;i<nextSetOfArguments.length;++i) {
      try {
        results.push(multiSmush.apply(null,nextSetOfArguments[i]));
      }
      catch(err) {
      }
    }

    return results[0];
  }
};

describe('word smushing', function() {
  describe('can smush', function() {
    it('should deal with single character smushes', function() {
      assert(canSmush('it','to'));
    });

    it('should deal with multiple overlaps', function() {
      assert(canSmush('bea', 'ear'));
    });

    it('shouldn\'t', function() {
      assert(!canSmush('be','to'));
      assert(!canSmush('never','anger'));
    });
  });

  describe('smush together', function() {
    it('wont smush unsmushable', function() {
      assert.throws(function() { smush('be','to'); });
    });

    it('joins together when 1 character overlaps', function() {
      var smushed = smush('it', 'to');
      assert.equal('ito', smushed);
    });

    it('should smush multiple overlaps', function() {
      var smushed = smush('bea', 'ear');
      assert.equal('bear', smushed);
    });

    it('should perform with test data', function() {
      var smushed = smush('testing', 'ginger');
      assert.equal('testinginger',smushed);
    });
  });

  describe('multiple smushings', function() {
    it('supports multiple arguments', function() {
      var smushed = multiSmush('eff');
      assert.equal('eff', smushed);
    });

    it('can join together', function() {
      var smushed = multiSmush('minutes', 'testing', 'ginger');
       assert.equal('minutestinginger', smushed);
    });

    it('works?', function() {
      var smushed = multiSmush(
	  'never', 
	  'eat', 
	  'peas', 
	  'in', 
	  'anger', 
	  'on', 
	  'vertical', 
	  'seating', 
	  'for',
	  'they', 
	  'easily', 
	  'form', 
	  'germs', 
	  'like', 
	  'german', 
	  'measles', 
	  'and', 
	  'mange');
      console.log(smushed);
    });
  });
});