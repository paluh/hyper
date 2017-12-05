// use strict

var stream = require('stream'),
    buffer = require('buffer'),
    util = require('util');

function MemoryWritableStream(writeDelay, options) {
  if (!(this instanceof MemoryWritableStream))
    return new MemoryWritableStream(options);
  this.writeDelay = writeDelay;
  this.output = new buffer.Buffer([]);
  stream.Writable.call(this, options);
}
util.inherits(MemoryWritableStream, stream.Writable);

MemoryWritableStream.prototype._write = function(chunk, encoding, cb) {
  var that = this;
  setTimeout(function() {
    that.output = Buffer.concat([that.output, chunk]);
    cb();
  }, this.writeDelay);
};

exports.memoryWritableStreamImpl = function(writeDelay) {
  return new MemoryWritableStream(writeDelay);
};

exports.streamBufferImpl = function(memoryWritableStream) {
  return memoryWritableStream.output;
};
