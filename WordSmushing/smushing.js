"use strict";

var assert = require('assert');

var smush = function(w1,w2) {
    if (w1 === '' || w2.indexOf(w1) === 0) return w2;
    return w1[0] + smush(w1.substr(1), w2);
};

describe('word smushing', function() {
    it('should join words with no overlap', function() {
	assert.equal('thecat', smush('the', 'cat'));
    });

    it('should do single character overlaps', function() {
	assert.equal('henot', smush('hen', 'not'));
    });

});
