/**
 * A class to draw a file chunks bar using HTML5 canvas.
 * NOTE: a fill color is defined here, but background color of canvas is defined in CSS.
 * @module utils/file-chunks-bar
 * @author Łukasz Opioła, Michal Borzecki
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import _ from 'lodash';

var canvasWidth = 320;
var canvasHeight = 20;
// Onedata colors
var fillColor = '#55E191';
function FileChunksBar(canvas, JSONData) {
  canvas = canvas[0];
  this.canvas = canvas;
  if (!canvas || !canvas.getContext) {
    throw 'no canvas or canvas context';
  }
  this.context = canvas.getContext('2d');
  if (!this.context) {
    throw 'no canvas 2d context';
  }
  this.fillColor = fillColor;
  this.canvas.width = canvasWidth;
  this.canvas.height = canvasHeight;
  this.draw(JSONData);
}
// JSON format: {"file_size": 1024, "chunks": [0, 100, 200, 300, 700, 1024]}
// Above means that file is 1024B big, and available chunks are {0, 100}, {200, 300} and {700, 1024}.
FileChunksBar.prototype.draw = function (data) {
  this.context.clearRect(0, 0, canvasWidth, canvasHeight);
  var fileSize = data.file_size;
  var chunks = data.chunks;
  var pixelsFill = _.times(canvasWidth, _.constant(0));
  var bytesPerPixel = fileSize / canvasWidth;
  for (var i = 0; i < chunks.length; i += 2) {
    var chunkStart = chunks[i];
    var chunkEnd = chunks[i + 1];
    var startPixel = Math.floor((chunkStart / fileSize) * canvasWidth);
    var endPixel = Math.floor((chunkEnd / fileSize) * canvasWidth);
    for (var pixel = startPixel; pixel <= endPixel; pixel++) {
      var lowerBound = Math.max(chunkStart, (pixel / canvasWidth) * fileSize);
      var upperBound = Math.min(chunkEnd, ((pixel + 1) / canvasWidth) * fileSize);
      var fillDelta = upperBound - lowerBound;
      pixelsFill[pixel] += fillDelta;
    }
  }
  pixelsFill.forEach((value, i) => this.drawPixelColumn(i, value / bytesPerPixel));
};

FileChunksBar.prototype.drawPixelColumn = function (column, opacity) {
  this.context.globalAlpha = opacity;
  this.context.fillStyle = this.fillColor;
  this.context.fillRect(column, 0, 1, canvasHeight);
};

export default FileChunksBar;
