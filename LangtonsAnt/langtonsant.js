"use strict";

var assert = require('assert');
var fs = require('fs');
var PNG = require('pngjs').PNG;

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

    this.flipWhite = function() {
	blackSquares.push({
	    x: ant.position().x,
	    y: ant.position().y
	});
	assert(this.color(ant.position()) === Color.Black);
    };

    this.flipBlack = function() {
	var n = blackSquares.length;
	blackSquares = blackSquares.filter(function(x) {
	    return !eqPoint(x, ant.position());
	});
	assert.equal(n - 1, blackSquares.length);
	assert(this.color(ant.position()) === Color.White);
    };

    this.step = function() {
	if (this.color(ant.position()) === Color.White) {
	    this.flipWhite();
	    ant.right();	
	} else {
	    this.flipBlack();
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
	var png = new PNG({
	    width: this.width(),
	    height: this.height(),
	}).on('close', function() {
	    console.log('closed');
	    callBack();
	});

	// Make all pixels white
	var size = png.width * png.height;
	for (var i=0;i<size;++i) {
	    png.data[i*4+0] = 255;
	    png.data[i*4+1] = 255;
	    png.data[i*4+2] = 255;
	    png.data[i*4+3] = 255;
	}

	// Set the black pixels up
	for (var i=0;i<blackSquares.length;++i) {
	    var lx = blackSquares[i].x + (png.width / 2);
	    var ly = blackSquares[i].y + (png.height / 2);

	    var offset = (lx * png.width) + ly;

	    png.data[offset * 4 + 0] = 0;
	    png.data[offset * 4 + 1] = 0;
	    png.data[offset * 4 + 2] = 0;
	    png.data[offset * 4 + 3] = 255;
	}

	png.pack().pipe(fs.createWriteStream(path));
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
