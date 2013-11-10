"use strict";

var Model = function(predictionContext, tokens) {

    var n = tokens.length;

    this._data = [];

    for (var i=0;i<n-1;++i) {
	var token = tokens[i];

	if (!this._data[token]) this._data[token] = {};

	if (!this._data[token][tokens[i+1]])
	    this._data[token][tokens[i+1]] = 0;

	this._data[token][tokens[i+1]]++;
    }

    this.entryCount = function() {
	return tokens.length;
    };

    this.frequencyOf = function() {
	return this._data[arguments[0]][arguments[1]];
    };

    return this;
};

var buildModel = function(predictionContext, source) { 
    return new Model(predictionContext, source); 
};

exports.buildModel = buildModel;
