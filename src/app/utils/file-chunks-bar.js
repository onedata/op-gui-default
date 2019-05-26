/**
 * A class to draw a file chunks bar using HTML5 canvas.
 * NOTE: a fill color is defined here, but background color of canvas is defined in CSS.
 * @module utils/file-chunks-bar
 * @author Łukasz Opioła, Michal Borzecki, Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

function computeOpacity(percentage) {
  return percentage <= 0 ? 0 : (percentage * 0.9 + 10) / 100;
}
 
// Onedata colors
const fillColor = '#55E191';
class FileChunksBar {
  constructor(canvas, JSONData) {
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
    this.canvas.width = JSONData.width || 320;
    this.canvas.height = JSONData.height || 20;
    this.draw(JSONData.chunksBarData);
  }
  // JSON format: { 0: 100, 50: 25, 200: 75 }
  // Above means that file is 1024 B big, and available chunks are in range 0-319:
  // - 0..50 (100%),
  // - 50..200 (25%),
  // - 200..end (75%).
  draw(chunksBarData) {
    const width = this.canvas.width;
    const height = this.canvas.height;
    this.context.clearRect(0, 0, width, height);
    
    let fragmentStarts = Object.keys(chunksBarData).map(x => parseInt(x));
    fragmentStarts.sort((x, y) => x - y);
    if (fragmentStarts.length !== 0 &&
      fragmentStarts[fragmentStarts.length - 1] !== width) {
        fragmentStarts.push(width);
    }
    
    for (let i = 0; i < fragmentStarts.length - 1; i++) {
      const startPixel = fragmentStarts[i];
      const endPixel = fragmentStarts[i + 1];
      const opacity = computeOpacity(chunksBarData[String(startPixel)]);
      for (let p = startPixel; p < endPixel; p++) {
        this.drawPixelColumn(p, opacity);
      }
    }
  }
  drawPixelColumn(column, opacity) {
    this.context.globalAlpha = opacity;
    this.context.fillStyle = this.fillColor;
    this.context.fillRect(column, 0, 1, this.canvas.height);
  }
}


export default FileChunksBar;
