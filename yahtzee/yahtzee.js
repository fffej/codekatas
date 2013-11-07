"use strict";

var assert = require('assert');

var valueIs = function(x) {
    return function(p) { return x === p; };
};

var onlyThose = function(dice,x) {
    return dice.filter(valueIs(x));
};

var sum = function(x,y) { return x + y; };

var Categories = {
    Chance:  function(dice) { return dice.reduce(sum); },
    Ones:    function(dice) { return onlyThose(dice,1).reduce(sum); },
    Twos:    function(dice) { return onlyThose(dice,2).reduce(sum); },
    Threes:  function(dice) { return onlyThose(dice,3).reduce(sum); },
    Pair:    function(dice) { 
	var maxValues = onlyThose(dice, Math.max.apply(null,dice));
	return maxValues.length === 2 ? (maxValues[0] * 2) : 0;
    }
};

var score = function(category, a,b,c,d,e) {
    var dice = [a,b,c,d,e];
    return category(dice);
};

describe('yahtzee', function() {
    describe('scoring', function() {
	it('should score chance as the sum of the dice', function() {
	    assert.equal(1+2+3+4+5, score(Categories.Chance, 1,2,3,4,5));
	});

	it('should score \'ones\' as the sum of those matching ones', function() {
	    assert.equal(3, score(Categories.Ones, 1,1,1,2,3));
	});

	it('should score \'twos\' as the sum of those matching twos', function() {
	    assert.equal(2, score(Categories.Twos, 1,1,1,2,3));
	});

	it('should score \'threes\' as the sum of those matching threes', function() {
	    assert.equal(3, score(Categories.Threes, 1,1,1,2,3));
	});

	it('should score the highest matching pair', function() {
	    assert.equal(8, score(Categories.Pair, 3,3,3,4,4));
	    assert.equal(0, score(Categories.Pair, 3,3,3,4,1));
	});

    });
});
