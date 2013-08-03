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

var Basket = function() {
  var items = [];
  return {
  
  };
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
    applies: function(basket, receipt) {
      if (numberOfItemsMatchingInBasket(item, basket) >= 1) {
        receipt.record(item.name(), item.unitPrice());
        return true;
      } else {
        return false;
      }
    }
  };
};

var BuyOneGetOneFree = function(item) {
  return {
    applies: function(basket, receipt) {
      if (numberOfItemsMatchingInBasket(item,basket) >= 2) {
        receipt.record("BOGOF " + item.name(), item.unitPrice());
        return true;
      } else {
        return false;
      }
    }
  };
};

var ThreeForTwo = function(item) {
  return {
    applies: function(basket, receipt) {
      if (numberOfItemsMatchingInBasket(item,basket) >= 3) {
        receipt.record("3 for 2 " + item.name(), 2 * item.unitPrice());
        return true;
      } else {
        return false;
      }
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

  describe('basket', function() {
    it('can keep track of items', function() {
      var basket = new Basket();
      basket.add(beans);

      assert.equal(1, basket.count());

      basket.remove(beans);
      assert.equal(0, basket.count());
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

    var item, price, receipt;

    beforeEach(function() {
      item = undefined;
      price = undefined;
      receipt = {
        record: function(item_, price_) {
          item = item_;
          price = price_;
        }
      };
    });

    it('unit offer', function() {
      var offer = new Unit(beans);
      assert(offer.applies([beans], receipt));
      assert.equal(45, price);
    });


    it('buy-one-get-one-free', function() {
      var offer = new BuyOneGetOneFree(beans);
      assert(offer.applies([beans, beans], receipt));
      assert.equal(45, price);
    });

    it('3 for 2', function() {
      var offer = new ThreeForTwo(beans);
      assert(offer.applies([beans,beans,beans], receipt));
      assert.equal(90, price);
    });
  });
});
