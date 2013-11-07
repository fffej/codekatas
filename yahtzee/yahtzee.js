"use strict";

var assert = require('assert');

var valueIs = function(x) {
    return function(p) { return x === p; };
};

var sum = function(x,y) { return x + y; };

var Categories = {
    Chance: function(dice) { return dice.reduce(sum); },
    Ones:   function(dice) { return dice.filter(valueIs(1)).reduce(sum); },
    Twos:   function(dice) { return dice.filter(valueIs(2)).reduce(sum); }
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

    });
});
