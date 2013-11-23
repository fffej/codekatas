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
	switch (orientation) {
	    case Orientation.Up: position.y += 1; break;
	    case Orientation.Left: position.x -= 1; break;
	    case Orientation.Down: position.y -= 1; break;
	    case Orientation.Right: position.x += 1; break;
	}
    };

    return this;
};


var makeAnt = function() {
    return new Ant();
};

var eqPoint = function(p1,p2) {
    return p1.x === p2.x && p1.y === p2.y;
};

var Game = function() {
    var ant = makeAnt();

    var blackSquares = [];

    this.color = function(key) {
	var n = blackSquares.length;
	for (var i=0;i<n;++i)
	    if (eqPoint(blackSquares[i],key)) 
		return Color.Black;

	return Color.White;
    };

    this.ant = function() {
	return ant;
    };

    this.flipAntSquareColor = function() {
	var isWhite = this.color(ant.position()) === Color.White;
	if (isWhite) {
	    blackSquares.push({
		x: ant.position().x,
		y: ant.position().y
	    });
	    assert(this.color(ant.position()) === Color.Black);
	} else {
	    blackSquares = blackSquares.filter(function(x) {
		return eqPoint(x, ant.position());
	    });
	    assert(this.color(ant.position()) === Color.White);
	}
    };

    this.step = function() {
	if (this.color(ant.position()) === Color.White) {
	    this.flipAntSquareColor();
	    ant.right();	
	} else {
	    this.flipAntSquareColor();
	    ant.left();
	}

	ant.move();
    };

    this.width = function() {
	var minX = blackSquares[0].x,
	    maxX = blackSquares[0].x;

	for (var i=0;i<blackSquares.length;++i) {
	    minX = Math.min(minX, blackSquares[i].x);
	    maxX = Math.max(maxX, blackSquares[i].x);
	}

	return maxX - minX;
    };

    this.height = function() {
	var minY = blackSquares[0].y,
	    maxY = blackSquares[0].y;

	for (var i=0;i<blackSquares.length;++i) {
	    minY = Math.min(minY, blackSquares[i].y);
	    maxY = Math.max(maxY, blackSquares[i].y);
	}

	return maxY - minY;
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
	    assert.deepEqual({x: 1, y: 0}, game.ant().position());

	    game.step();
	    assert.equal(Orientation.Down, game.ant().orientation());
	    assert.deepEqual({x: 1, y: -1}, game.ant().position());
	});

	it('flips square color', function() {
	    var game = makeGame();
	    assert.equal(Color.White, game.color({x: 0, y: 0}));

	    game.step();
	    assert.equal(Color.Black, game.color({x: 0, y: 0}));
	});

	it('steps multiple times', function() {
	    var game = makeGame();
	    for (var i=0;i<200;++i) 
		game.step();

	    assert.equal(1, game.width());
	    assert.equal(1, game.height());
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

	    ant.left(); ant.move();
	    assert.equal(-1, ant.position().x);

	    ant.left(); ant.move();
	    assert.equal(0, ant.position().y);

	    ant.left(); ant.move();
	    assert.equal(0, ant.position().x);
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

	    ant.left();
	    assert.equal(Orientation.Left, ant.orientation());
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
});
