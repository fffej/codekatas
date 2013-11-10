"use strict";


var createData = function(pc, tokens) {
    var data = {},
        n = tokens.length;

    for (var i=0;i<n-(pc-1);++i) {
	var token = tokens[i];

	if (!data[token]) data[token] = {};

	for (var j=0;j<pc;++j) {
	    if (!data[token][tokens[i+j]])
		data[token][tokens[i+j]] = 0;
	}

	data[token][tokens[i+1]]++;
    }

    return data;
};

var Model = function(predictionContext, tokens) {

    var n = tokens.length;

    this._data = createData(predictionContext, tokens);

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
