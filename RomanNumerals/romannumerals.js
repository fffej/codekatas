"use strict";

var assert = require('assert');

var toRomanNumeral = function(str) {

};

describe('Roman Numerals', function() {
  describe('Acceptance Test', function() {
    it('meets acceptance criteria', function() {
      assert.equal("MCMXC", toRomanNumeral(1990));
      assert.equal("MMVIII", toRomanNumeral(2008));
      assert.equal("XCIX", toRomanNumeral(99));
      assert.equal("XLVII", toRomanNumeral(47));
    });
  });
});
