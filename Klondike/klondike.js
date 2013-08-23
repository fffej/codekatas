"use strict";
var assert = require('assert');

var Suit = {
  Spades: 1,
  Diamonds: 2,
  Clubs: 3,
  Hearts: 4
};

var Stack = function() {

  Stack.prototype._cards = [];

  Stack.prototype.empty = function() {
    return true;
  };

  Stack.prototype.add = function(card) {
    this._cards.push(card);
  };  

  Stack.prototype.top = function() {
    return this._cards[0];
  };

  return this;
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

    it('can add card', function() {
      var stack = new Stack();
      stack.add(new Card(Suit.Spades, 1));

      assert.deepEqual(new Card(Suit.Spades, 1), stack.top());
    });
  });

});
