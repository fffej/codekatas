"use strict";

var assert = require('assert');

var Item = function(name, price) {
  return {
    name: function() {
      return name;
    },
    unitPrice: function() {
      return price;
    }
  }
};

var Register = function() {
  
  var runningSum = 0;
  var offers = [];
  var items = [];

  return {
     price: function(item) {
       items.push(item);
     },

     insertOffer: function(offer) {
       offers.push(offer);
     },

     total: function() {
       var runningSum = 0;
       for (var i=0;i<items.length;++i) {
         runningSum += items[i].unitPrice();
       }

       return runningSum;
     } 
  };
};

var numberOfItemsMatchingInBasket = function(item, basket) {
  var count = 0;
  for (var i=0;i<basket.length;++i) {
    if (basket[i].name() === item.name()) {
      count++;
    }
  }
 
  return count;
};

var Receipt = function() {
  var items = [];
  var total = 0;

  return {
    items: function() { return items; },

    total: function() { return total; },

    record: function(description, price) {
      items.push(description + ' : ' + price);
      total += price;
    },
  };
};

var Unit = function(item) {
  return {
    applies: function(basket) {
      return numberOfItemsMatchingInBasket(item, basket) >= 1;
    }
  };
};

var BuyOneGetOneFree = function(item) {
  return {
    applies: function(basket) {
      return numberOfItemsMatchingInBasket(item,basket) >= 2;
    }
  };
};

var ThreeForTwo = function(item) {
  return {
    applies: function(basket) {
      return numberOfItemsMatchingInBasket(item,basket) >= 3;
    }
  }
};

describe('super market', function() {

  var beans = new Item('beans', 45);
  var batteries = new Item('batteries', 60);

  describe('item', function() {
    it('have a unit price', function() {
      assert.equal(45, beans.unitPrice());
    });
  });

  describe('receipt', function() {
    it('consists of items and a total price', function() {
      var receipt = new Receipt();
      receipt.record('2 x Beans', 45);
      receipt.record('4 x Shoes', 10);

      assert.equal(2, receipt.items().length);
      assert.equal(55, receipt.total());
    });
  });

  describe('register', function() {
    it('totals the price of items', function() {
      var register = new Register();
      register.price(beans);
      register.price(beans);

      assert.equal(90, register.total());
    });

    it('knows of offers', function() {
      var register = new Register();
      register.insertOffer(new BuyOneGetOneFree(beans));

      register.price(beans);
      register.price(beans);

      assert.equal(45, register.total());
    });
  });

  describe('offer', function() {
 
    var item;
    var price;
    var receipt = {
      record: function(item_, price_) {
        item = item_;
        price = price_;
      }
    };
 
    beforeEach(function() {
      item = undefined;
      price = undefined;
    });

    it('unit offer', function() {
      var offer = new Unit(beans);
      assert(offer.applies([beans], receipt));
    });


    it('buy-one-get-one-free', function() {
      var offer = new BuyOneGetOneFree(beans);
      assert(offer.applies([beans, beans], receipt));
    });

    it('3 for 2', function() {
      var offer = new ThreeForTwo(beans);
      assert.equal(false, offer.applies([beans,beans]));
      assert(offer.applies([beans,beans,beans], receipt));
    });
  });
});
