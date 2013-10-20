"use strict";

var assert = require('assert');

var Game = function(goal) {

    var scoreBulls = function(guess, goal) {
	var bulls = 0;
	for (var i=0;i<guess.length;++i) {
	    if (guess[i] === goal[i])
		bulls++;
	}
	return bulls;
    };

    var scoreCows = function(guess, goal) {
	var cows = 0;
	for (var i=0;i<goal.length;++i) {
	    var desiredChar = goal[i];
	    for (var j=0;j<goal.length;++j) {
		if (j === i) continue;

		if (guess[j] === desiredChar) {
		    cows++;
		}
	    }
	}
	return cows;
    };

    this.score = function(guess) {
	assert(goal.length === guess.length, 'Lengths must be equivalent');

        return {
	    cows: scoreCows(guess,goal),
	    bulls: scoreBulls(guess,goal)
	};
    };

    var finishingScore = function(score) {
	return score.bulls === 4;
    };

    this.play = function(strategy) {
	var turns = 0;

	do {
	    turns++;
	    var guess = strategy.guess();
	    var score = this.score(guess);
	    strategy.update(guess,score);

	} while (!finishingScore(score) && turns < 10002);

	return turns;
    };

    return this;
};

var asString = function(num) {
    var s = '0000' + num;
    return s.substr(s.length - 4);
};

var fromTo = function(start,end) {
    var arr = [];
    for (var i=start;i<=end;++i) {
	arr.push(i);
    }
    return arr;
};

var Player = function() {

    this.possibilities = fromTo(0,9999);

    this.guess = function() {
	var n = (Math.random() * this.possibilities.length) | 0;
	var guess = asString(this.possibilities[n]);

	return guess;
    };

    this.eliminateMatches = function(guess, fn) {
	for (var i=0;i<guess.length;++i) {
	    var digit = guess[i];
	    for (var k=this.possibilities.length-1;k>=0;--k) {
		var possibility = this.possibilities[k];
		if (fn(asString(possibility),digit)) {
		    this.possibilities.splice(k,1);
		}
	    }
	}
    };

    this.eliminatePartialMatches = function(guess, n) {
	for (var k=this.possibilities.length-1;k>=0;--k) {
	    var poss = asString(this.possibilities[k]);
	    var foundCount = 0;

	    // poss must contain at least n characters from guess
	    for (var i=0;i<4;++i) {
		var goalChar = guess[i];
		for (var j=0;j<4;++j) {
		    if (poss[j] === goalChar) {
			foundCount++;
			poss[j] = -1; // so we don't count it again
			break;
		    }
		}
	    }
	    if (foundCount < n)
		this.possibilities.splice(k,1);
	}
    };



    this.update = function(guess,score) {
	if (score.cows + score.bulls === 0) {
	    // None of the numbers are correct so we can eliminate anything
	    // that contains any of these digits
	    this.eliminateMatches(guess, function(str,digit) {
		return str.indexOf(digit) !== -1;
	    });
	}

	if (score.bulls > 0) {
	    // Any possibility that doesn't contain at least
	    // score.bulls in the same position as guess is wrong
	    for (var i=this.possibilities.length-1; i>=0; --i) {
		var p = asString(this.possibilities[i]);
		
		var inRightPlace = 0;
		for (var k=0;k<4;++k) {
		    inRightPlace += (p[k] === guess[k]) ? 1 : 0;
		}

		if (inRightPlace < score.bulls) {
		    this.possibilities.splice(i,1);
		}
	    }
	}

	if (score.cows + score.bulls > 0) {
	    // Any possibility that doesn't contain at least 
	    // score.cows + score.bulls from guess is wrong
	    this.eliminatePartialMatches(guess, score.cows + score.bulls);
	}
    };

    return this;
};

var Naive = function() {

    this.currentGuess = 0;

    this.guess = function() {
	return asString(this.currentGuess);
    };

    this.update = function(guess, score) {
	this.currentGuess++;
    };

    return this;
};

var createGame = function(goal) {
    return new Game(goal);
};

var containsDuplicates = function(str) {
    for (var i=0;i<str.length;++i) {
	var found = false;
	for (var j=0;j<str.length;++j) {
	    if (i === j) continue;
	    
	    if (str[i] === str[j])
		return true;
	}
    }
    return false;
};

describe("cows and bulls", function() {

    describe('playing naive', function() {
	it('should solve it within 9999 turns', function() {
	    var game = createGame('5555');
	    var turns = game.play(new Naive());

	    assert.equal(5556, turns);
	});
    });

    describe('slightly intelligently playing', function() {

	it('should solve the game within 9999 turns', function() {
	    for (var i=0;i<100;++i) {
		var goal = asString(Math.random() * 10000 | 0);
		var game = createGame(goal);
		var turns = game.play(new Player());
	    
		assert(turns < 10000, 'Too many turns to solve: ' + goal);
	    }
	});

	it('initial pool of answers is 10000', function() {
	    var player = new Player();

	    assert.equal(10000, player.possibilities.length);
	});

	it('zero scores eliminate possibilities', function() {
	    var player = new Player();

	    player.update('1111', { cows: 0, bulls: 0 });

	    assert.equal(6561, player.possibilities.length);
	});

	it('correct guesses eliminate possibilities', function() {
	    var player = new Player();

	    player.update('1234', { cows: 4, bulls: 0 });

	    assert.equal(4*3*2*1, player.possibilities.length);
	});

	it('partial bull matches eliminate many possibilities', function() {
	    var player = new Player();

	    player.update('1234', { cows: 0, bulls: 1 });

	    // all possibilities must have at least one in the right position
	    for (var i=0;i<player.possibilities.length;++i) {
		var remaining = asString(player.possibilities[i]);

		assert(remaining[0] === '1' ||
		       remaining[1] === '2' ||
		       remaining[2] === '3' ||
		       remaining[3] === '4');
	    }
	});

	it('partial matches eliminate possibilites', function() {
	    var player = new Player();

	    player.update('1234', { cows: 2, bulls: 0 });

	    for (var i=0;i<player.possibilities.length;++i) {
		var v = asString(player.possibilities[i]);
		
		var count = 0;
		for (var k=1;k<=4;++k) 
		    count += v.indexOf(k + '') !== -1 ? 1 : 0;
		assert(count >= 2);
	    }
	});

	it('partial matches eliminate possibilities (2)', function() {
	    var player = new Player();
	    
	    player.update('1111', { cows: 1, bulls: 0 });
	    assert.equal(3439, player.possibilities.length);
	    for (var i=0;i<player.possibilities.length;++i) {
		assert(-1 != asString(player.possibilities[i]).indexOf('1'));
	    }
	});
    });

    describe('scoring', function() {
	it('should match the example', function() {
	    var game = createGame('8045');
	    var result = game.score('0865');

	    assert.equal(2, result.cows);
	    assert.equal(1, result.bulls);
	});

	it('no matches scores no cows or bulls', function() {
            var game = createGame('1234');

            var r = game.score('5678');
            assert.equal(0,r.cows);
	    assert.equal(0,r.bulls);
	});

	it('all correct is all bulls', function() {
	    var game = createGame('1234');

	    var r = game.score('1234');
	    assert.equal(4, r.bulls);
	    assert.equal(0, r.cows);
	});

	it('all in wrong order is all cows', function() {
	    var game = createGame('1234');

	    var r = game.score('4321');
	    assert.equal(4, r.cows);
	    assert.equal(0, r.bulls);
	});
    });
});
