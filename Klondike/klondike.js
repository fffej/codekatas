"use strict";
var assert = require('assert');

describe('klondike', function() {
  describe('card', function() {
    it('has a suit and a value', function() {
      var card = new Card(Suit.Spades);
      assert.equal(Suit.Spades, card.suit());
      assert.equal(Value.Ace, card.value());
    });
  });
});
