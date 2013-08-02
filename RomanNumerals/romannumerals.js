"use strict";

var assert = require('assert');

var toRomanNumeral = function(str) {

};

var RomanLetter = function(symbol,value) {
  return {
    value: function() { return value; },
    symbol: function() { return symbol; }
  };
};

var RomanAlphabet = function() {
  var letters = [
    new RomanLetter(1,'I'),
    new RomanLetter(2,'II'),
    new RomanLetter(3,'III'),
    new RomanLetter(4,'IV'),
    new RomanLetter(5,'V'),
    new RomanLetter(6,'VI'),
    new RomanLetter(7, 'VII'),
    new RomanLetter(8, 'VIII'),
    new RomanLetter(9, 'IX'),
    new RomanLetter(10,'X'),
    new RomanLetter(20,'XX'),
    new RomanLetter(30,'XXX'),
    new RomanLetter(40,'XL'),
    new RomanLetter(50,'L'),
    new RomanLetter(60,'LX'),
    new RomanLetter(70, 'LXX'),
    new RomanLetter(80, 'LXXX'),
    new RomanLetter(90, 'XC'),
    new RomanLetter(100,'C'),
    new RomanLetter(200,'CC'),
    new RomanLetter(300,'CCC'),
    new RomanLetter(400,'CD'),
    new RomanLetter(500,'D'),
    new RomanLetter(600,'DC'),
    new RomanLetter(700, 'DCC'),
    new RomanLetter(800, 'DCCC'),
    new RomanLetter(900, 'CM'),
    new RomanLetter(1000, 'M'),
    new RomanLetter(2000, 'MM'),
    new RomanLetter(3000, 'MMM'),
    new RomanLetter(4000, 'MMMM'),
  ];

  return {
    count: function() { return letters.length; }
  }
};

describe('Roman Numerals', function() {
  describe('basics', function() {

    it('roman letters have a symbol and value', function() {
      var romanLetter = new RomanLetter('I',1);  
      assert.equal(1, romanLetter.value());
      assert.equal('I', romanLetter.symbol());
    });

    it('roman alphabet consists of letters', function() {
      var romanAlphabet = new RomanAlphabet();

      assert.equal("I", romanAlphabet.symbolFor(1));
      assert.equal("V", romanAlphabet.symbolFor(5));
      assert.equal("M", romanAlphabet.symbolFor(1000));
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
