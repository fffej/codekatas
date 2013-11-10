"use strict";

var ensurePropertyExists = function(d,i) {
    if (!d[i]) d[i] = {};
    return d[i];
}

var createData = function(pc, tokens) {
    var data = {},
        n = tokens.length;

    for (var i=0;i<n-(pc-1);++i) {
	var obj = ensurePropertyExists(data,tokens[i]);

	for (var j=1;j<pc-1;++j) {
	    obj[tokens[i+j]] = {};
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

    this.pickOne = function(soFar, upTo) {
	var n = soFar.length;
	var obj = this._data[soFar[n-1]];

	return this.weightedChoice(obj);
    };

    this.weightedChoice = function(obj) {

	var words = [];

	for(var prop in obj) {
	    for (var i=0;i<obj[prop];++i) {
		words.push(prop);
	    }
	}
	
	var n = Math.random() * words.length | 0;
	return words[n];
    };

    this.generate = function(start, len) {
	var r = [];
	r[0] = tokens[start];

	for (var i=1;i<len;++i) {
	    r[i] = this.pickOne(r,i-1);
	}

	return r;
    };

    return this;
};

var buildModel = function(predictionContext, source) { 
    return new Model(predictionContext, source); 
};

exports.buildModel = buildModel;
