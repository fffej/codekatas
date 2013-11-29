"use strict";
var assert = require('assert');
var fs = require('fs');

var langton = require('./langtonsant');

describe('langton\'s ant', function() {

    describe('game', function() {
	it('exists', function() {
	    assert(langton.makeGame());
	});

	it('records state', function() {
	    var game = langton.makeGame();
	    assert.deepEqual({x: 0, y: 0}, game.ant().position());
	});

	it('steps ant orientation', function() {
	    var game = langton.makeGame();
	    game.step();

	    assert.equal(langton.Orientation.Right, game.ant().orientation());
	    assert.deepEqual({x: 1, y: 0}, game.ant().position());

	    game.step();
	    assert.equal(langton.Orientation.Down, game.ant().orientation());
	    assert.deepEqual({x: 1, y: -1}, game.ant().position());
	});

	it('flips square color', function() {
	    var game = langton.makeGame();
	    assert.equal(langton.Color.White, game.color({x: 0, y: 0}));

	    game.step();
	    assert.equal(langton.Color.Black, game.color({x: 0, y: 0}));
	});

	it('steps multiple times', function() {
	    var game = langton.makeGame();
	    for (var i=0;i<200;++i) 
		game.step();

	    assert.equal(8, game.width());
	    assert.equal(8, game.height());
	});

	it('exports to png', function() {
	    var game = langton.makeGame();
	    for (var i=0;i<200;++i) game.step();

	    var calledBack = false;

	    game.export('foo.png', function() {
		calledBack = true;
		assert(fs.existsSync('foo.png'));
	    });

	    assert(calledBack);
	});
    });

    describe('ant', function() {
	it('exists', function() {
	    assert(langton.makeAnt());
	});

	it('has a location', function() {
	    var ant = langton.makeAnt();
	    assert.equal(0, ant.position().x);
	    assert.equal(0, ant.position().y);
	});

	it('orientation up', function() {
	    var ant = langton.makeAnt();
	    assert.equal(langton.Orientation.Up, ant.orientation());
	});

	it('moves', function() {
	    var ant = langton.makeAnt();
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
	    var ant = langton.makeAnt();
	    ant.left();
	    assert.equal(langton.Orientation.Left, ant.orientation());

	    ant.left();
	    assert.equal(langton.Orientation.Down, ant.orientation());

	    ant.left();
	    assert.equal(langton.Orientation.Right, ant.orientation());

	    ant.left();
	    assert.equal(langton.Orientation.Up, ant.orientation());

	    ant.left();
	    assert.equal(langton.Orientation.Left, ant.orientation());
	});

	it('turns right', function() {
	    var ant = langton.makeAnt();
	    ant.right();
	    assert.equal(langton.Orientation.Right, ant.orientation());

	    ant.right();
	    assert.equal(langton.Orientation.Down, ant.orientation());

	    ant.right();
	    assert.equal(langton.Orientation.Left, ant.orientation());

	    ant.right();
	    assert.equal(langton.Orientation.Up, ant.orientation());
	});
    });
});
