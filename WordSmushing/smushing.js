"use strict";

var assert = require('assert');

describe('word smushing', function() {
    it('should join words with no overlap', function() {
	assert.equal('thecat', smush('the', 'cat'));
    });
});
