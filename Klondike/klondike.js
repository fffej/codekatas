"use strict";
var assert = require('assert');

var Suit = {
  Spades: 1,
  Diamonds: 2,
  Clubs: 3,
  Hearts: 4
};

var Deck = function() {
  return this;
};

var Card = function(suit,value) {
  this._suit = suit;
  this._value = value;

  Card.prototype.suit = function() {
    return this._suit;
  };

  Card.prototype.value = function() {
    return this._value;
  };

  return this;
};

describe('klondike', function() {
  describe('card', function() {
    it('has a suit and a value', function() {
      var card = new Card(Suit.Spades,1);
      assert.equal(Suit.Spades, card.suit());
      assert.equal(1, card.value());
    });
  });

  describe('deck', function() {
    it('consists of 52 cards', function() {
      var deck = new Deck();
    });
  });

  describe('stack', function() {
    it('is a concept', function() {
      assert(new Stack().empty());
    });
  });

});
