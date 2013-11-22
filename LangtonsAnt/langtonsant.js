"use strict";
var assert = require('assert');

var Color = {
    White: 0,
    Black: 1
};

var Orientation = {
    Up: 0,
    Left: 1,
    Right: 2
};

var Ant = function() {
    
    var position = {x: 0, y: 0};
    var orientation = Orientation.Up;

    this.position = function() {
	return position;
    };

    this.orientation = function() {
	return orientation;
    };

    this.left = function() {
	
	var l = {};
	l[Orientation.Up] = Orientation.Left;
	l[Orientation.Left] =  Orientation.Down;
	l[Orientation.Down] =  Orientation.Right;
	l[Orientation.Right] =  Orientation.Up;

	orientation = l[orientation];
    };

    this.right = function() {
	
	var r = {};
	r[Orientation.Up] = Orientation.Right;
	r[Orientation.Left] =  Orientation.Up;
	r[Orientation.Down] =  Orientation.Left;
	r[Orientation.Right] =  Orientation.Down;

	orientation = r[orientation];
    };

    this.move = function() {
    };

    return this;
};


var makeAnt = function() {
    return new Ant();
};

var Square = function() {

    var color = Color.White;

    this.color = function() { 
	return color;
    };

    this.flip = function() {
	color = (color == Color.White ? Color.Black : Color.White);
    };

    return this;
};

var makeSquare = function() {
    return new Square();
};

var eqPoint = function(p1,p2) {
    return p1.x === p2.x && p1.y === p2.y;
};

var Game = function() {
    var ant = makeAnt();

    var blackSquares = [];

    this.color = function(key) {
	for (var i=0;i<blackSquares.length;++i)
	    if (eqPoint(blackSquares[i],key)) return Color.Black;

	return Color.White;
    };

    this.ant = function() {
	return ant;
    };

    this.step = function() {
	if (this.color(ant.position()) === Color.White) {
	    blackSquares.push(ant.position());
	    ant.right();	
	} else {
	    blackSquares.remove(ant.position());
	    ant.left();
	}
    };

    return this;
};


var makeGame = function() {
    return new Game();
};

describe('langton\'s ant', function() {

    describe('game', function() {
	it('exists', function() {
	    assert(makeGame());
	});

	it('records state', function() {
	    var game = makeGame();
	    assert.deepEqual({x: 0, y: 0}, game.ant().position());
	});

	it('steps ant orientation', function() {
	    var game = makeGame();
	    game.step();

	    assert.equal(Orientation.Right, game.ant().orientation());
	});
	
	it('flips square color', function() {
	    var game = makeGame();
	    assert.equal(Color.White, game.color({x: 0, y: 0}));

	    game.step();
	    assert.equal(Color.Black, game.color({x: 0, y: 0}));
	});
    });

    describe('ant', function() {
	it('exists', function() {
	    assert(makeAnt());
	});

	it('has a location', function() {
	    var ant = makeAnt();
	    assert.equal(0, ant.position().x);
	    assert.equal(0, ant.position().y);
	});

	it('orientation up', function() {
	    var ant = makeAnt();
	    assert.equal(Orientation.Up, ant.orientation());
	});

	it('moves', function() {
	    var ant = makeAnt();
	    ant.move();
	    assert.equal(0, ant.position().x);
	    assert.equal(1, ant.position().y);
	});

	it('turns left', function() {
	    var ant = makeAnt();
	    ant.left();
	    assert.equal(Orientation.Left, ant.orientation());

	    ant.left();
	    assert.equal(Orientation.Down, ant.orientation());

	    ant.left();
	    assert.equal(Orientation.Right, ant.orientation());

	    ant.left();
	    assert.equal(Orientation.Up, ant.orientation());
	});

	it('turns right', function() {
	    var ant = makeAnt();
	    ant.right();
	    assert.equal(Orientation.Right, ant.orientation());

	    ant.right();
	    assert.equal(Orientation.Down, ant.orientation());

	    ant.right();
	    assert.equal(Orientation.Left, ant.orientation());

	    ant.right();
	    assert.equal(Orientation.Up, ant.orientation());
	});
    });

    describe('square', function() {
	it('exists', function() {
	    assert(makeSquare());
	});

	it('initially white', function() {
	    assert.equal(Color.White, makeSquare().color());
	});

	it('flips', function() {
	    var sq = makeSquare();
	    sq.flip();
	    assert.equal(Color.Black, sq.color());
	});
    });
});
