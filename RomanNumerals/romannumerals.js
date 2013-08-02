"use strict";

var assert = require('assert');

/*
1 ->    "I" | 10 ->    "X" | 100 ->    "C" | 1000 ->    "M"
2 ->   "II" | 20 ->   "XX" | 200 ->   "CC" | 2000 ->   "MM"
3 ->  "III" | 30 ->  "XXX" | 300 ->  "CCC" | 3000 ->  "MMM"
4 ->   "IV" | 40 ->   "XL" | 400 ->   "CD" | 4000 -> "MMMM"
5 ->    "V" | 50 ->    "L" | 500 ->    "D"
6 ->   "VI" | 60 ->   "LX" | 600 ->   "DC" 
7 ->  "VII" | 70 ->  "LXX" | 700 ->  "DCC" 
8 -> "VIII" | 80 -> "LXXX" | 800 -> "DCCC" 
9 ->   "IX" | 90 ->   "XC" | 900 ->   "CM" 
*/

var toRomanNumeral = function(str) {

};

var RomanLetter = function(symbol,value) {
  return {
    value: function() { return value; },
    symbol: function() { return symbol; }
  };
};

describe('Roman Numerals', function() {
  describe('basics', function() {

    it('roman letters have a symbol and value', function() {
      var romanLetter = new RomanLetter('I',1);  
      assert.equal(1, romanLetter.value());
      assert.equal('I', romanLetter.symbol());
    });

    describe('Acceptance Test', function() {
      it('meets acceptance criteria', function() {
        assert.equal("MCMXC", toRomanNumeral(1990));
        assert.equal("MMVIII", toRomanNumeral(2008));
        assert.equal("XCIX", toRomanNumeral(99));
        assert.equal("XLVII", toRomanNumeral(47));
      });
    });
  });
});
