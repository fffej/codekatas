"use strict";


var createData = function(pc, tokens) {
    var data = {},
        n = tokens.length;

    for (var i=0;i<n-(pc-1);++i) {
	var token = tokens[i];

	if (!data[token]) data[token] = {};

	var obj = data[token];
	for (var j=1;j<pc-1;++j) {
	    obj[tokens[i+j]] = new Object();
	    obj = obj[tokens[i+j]];
	}

	if (!obj[tokens[i+pc-1]])
	    obj[tokens[i+pc-1]] = 0;

	obj[tokens[i+pc-1]]++;
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
	var end = this._data;

	for (var i=0;i<arguments.length;++i) {
	    end = end[arguments[i]];
	}

	return end;
    };

    return this;
};

var buildModel = function(predictionContext, source) { 
    return new Model(predictionContext, source); 
};

exports.buildModel = buildModel;
