"use strict";

var assert = require('assert');

describe('tennis', function() {
  describe('game', function() {
    it('starts at 0-0', function() {
      var game = new Game();
      assert.equal('0 - 0', game.score());
    });
  });
});
