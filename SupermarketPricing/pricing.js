"use strict";

var assert = require('assert');

var Item = function(name, price) {
  return {
    unitPrice: function() {
      return price;
    }
  }
};

var Register = function() {
  
  var runningSum = 0;

  return {
     price: function(item) {
       runningSum += item.unitPrice();
     },

     total: function() {
       return runningSum;
     } 
  };
};

describe('super market', function() {

  var beans = new Item('beans', 45);
  var batteries = new Item('batteries', 60);

  describe('item', function() {
    it('have a unit price', function() {
      assert.equal(45, beans.unitPrice());
    });
  });

  describe('register', function() {
    it('totals the price of items', function() {
      var register = new Register();
      register.price(beans);
      register.price(beans);

      assert.equal(90, register.total());
    });
  });

  describe('offer', function() {
    it('buy-one-get-one-free', function() {
      var offer = new BuyOneGetOneFree(beans);
      assert(offer.applies([beans, beans]));
    });
  });
});
