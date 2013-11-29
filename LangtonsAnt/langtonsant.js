"use strict";

var assert = require('assert');
var fs = require('fs');

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
	    var n = blackSquares.length;
	    blackSquares = blackSquares.filter(function(x) {
		return !eqPoint(x, ant.position());
	    });
	    assert.equal(n - 1, blackSquares.length);
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

    this.export = function(path, callBack) {
	fs.writeFile(path, 'not valid', function(err) {
	    if (err) throw err;
	});
	callBack();
    };

    return this;
};


var makeGame = function() {
    return new Game();
};

exports.makeGame = makeGame;
exports.makeAnt = makeAnt;
exports.Orientation = Orientation;
exports.Color = Color;
