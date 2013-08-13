"use strict";

var assert = require('assert');

var Item = function(desc) {

  var items = [];

  this.description = desc;

  return this;
};

var Basket = function() {

  var items = [];

  this.add = function(item) {
    items.push(item);
  };

  this.contents = function() {
    return items;
  };

  return this;
};

var Checkout = function() {

  this.price = function(basket) {
    return 0;
  };

  return this;
};

var Unit = function(item,price) {

  this.applies = function(basket) {
    return true;
  };

  return this;
};

describe('shopping basket', function() {
  describe('item', function() {
    it('exists', function() {
      assert(new Item('description'));
    });

    it('has a description', function() {
      var item = new Item('banana');
      assert.equal('banana', item.description);
    });
  });

  describe('basket', function() {
    it('exists', function() {
      assert(new Basket());
    });

    it('items can be added', function() {
      var basket = new Basket();
      basket.add(new Item('Shoes'));

      assert.equal(1, basket.contents().length);
    });

    it('can be queried', function() {
      var basket = new Basket();

      var items = basket.itemsMatching(
        function(x) {
          return false;
        }
      );

      assert.equal(0, items.length);
    });
  });

  describe('checkout', function() {
    it('exists', function() {
      assert(new Checkout());
    });

    it('prices an empty basket at zero', function() {
      var checkout = new Checkout();
      assert.equal(0, checkout.price(new Basket()));
    });
  });

  describe('offers', function() {
    it('unit', function() {
      var beans = new Item('beans');
      var basket = new Basket();
      var unit = new Unit(beans, 50);      

      basket.add(beans);

      assert(unit.applies(basket));
    });
  });
});
