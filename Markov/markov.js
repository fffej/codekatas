"use strict";

var Model = function(tokens) {

    this.entryCount = function() {
	return tokens.length;
    };

    this.frequencyOf = function() {
	return 1;
    };

    return this;
};

var buildModel = function(source) { 
    return new Model(source); 
};

exports.buildModel = buildModel;
