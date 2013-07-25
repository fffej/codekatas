var assert = require("assert");

var bitset = function(value,bit) {
  return !!(value & (1 << bit));
};

var bitsSet = function(value) {
  var bits = numberOfBits(value);
  var ret = [];
  for (var i=0;i<bits;++i) {
    ret.push(bitset(value,i) ? 1 : 0);
  }
  return ret;
};

var numberOfBits = function(value) {
  if (value <= 2) {
    return value;
  } 

  var log2 = Math.log(value) / Math.LN2;
  var maxVal = Math.pow(2,log2);
  return (maxVal >= value) ? log2 : log2 + 1;
};

var hasAdjacentElements = function(arr,needle) {
  if (arr.length < 2) {
    return false;
  } 

  for (var i=0;i<arr.length-1;++i) {
    if (arr[i] === arr[i+1] && arr[i] === needle) { 
      return true;
    }
  }
  return false;
};

var adjacentCounts = function(bitCount) {
  var maxNum = Math.pow(2,bitCount);
  var count = 0; 
  for (var i=0;i<maxNum;++i) {
    if (hasAdjacentElements(bitsSet(i),1)) {
      console.log(i);
      count++;
    }
  }
  return count;
};

describe('Diversion', function() {
  it('has a bit that is set', function() {
    assert(bitset(1,0));
    assert(bitset(256,8));
  });

  it('number of bits within a value', function() {
    assert.equal(8, numberOfBits(256));
    assert.equal(1, numberOfBits(1));
    assert.equal(2, numberOfBits(2));
    assert.equal(0, numberOfBits(0));
  });

  it('has bits that are not set', function() {
    assert(!bitset(257,7));
  });

  it('returns the binary representation', function() {
    assert.deepEqual([], bitsSet(0));
    assert.deepEqual([0,1], bitsSet(2));
    assert.deepEqual([1,0,1,0,0,0,0,1], bitsSet(133));
  });

  it('bit shift operations', function() {
    assert.equal(1, 1 << 0);
    assert.equal(2, 1 << 1);
    assert.equal(4, 1 << 2);
  });

  it('can detect adjacent elements', function() {
    assert.equal(false, hasAdjacentElements([],1));
    assert.equal(false, hasAdjacentElements([1],1));
    assert.equal(true, hasAdjacentElements([1,1],1));
    assert.equal(false, hasAdjacentElements([1,2,3],0));
    assert.equal(false, hasAdjacentElements([0,1,1,0],0));
    assert.equal(true, hasAdjacentElements([0,1,1,0],1));
  });

  it('n digit numbers that don\'t have adjacent bits', function() {
    assert.equal(5, adjacentCounts(3));
  });
});