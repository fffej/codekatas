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
    return this._cards[this._cards.length - 1];
  };

  Stack.prototype.pop = function() {
    return this._cards.pop();
  };

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

  describe('foundation', function() {
    it('has four stacks', function() {
      var foundation = new Foundation();
      assert(foundation[Suit.Spades].empty());
      assert(foundation[Suit.Hearts].empty());
      assert(foundation[Suit.Diamonds].empty());
      assert(foundation[Suit.Clubs].empty());
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

    var stackWithTwoCards = function() {
      var stack = new Stack();
      stack.add(new Card(Suit.Spades, 1));
      stack.add(new Card(Suit.Hearts, 2));
 
      return stack;
    };

    it('can add multiple cards', function() {
      var stack = stackWithTwoCards();
      assert.deepEqual(new Card(Suit.Hearts, 2), stack.top());
    });

    it('can pop cards', function() {
      var stack = stackWithTwoCards();
      stack.pop();
      assert.deepEqual(new Card(Suit.Spades, 1), stack.top());
    });
  });

});
