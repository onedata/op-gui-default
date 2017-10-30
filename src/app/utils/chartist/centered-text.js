/**
 * Plugin for Chartist which adds additional text in the center of the chart.
 * 
 * Options:
 * * text - text to display
 * * fontSize - [optional] text font size (percents will be relative 
 *   to `min(chart.width, chart.height)`)
 * * maxWidth - [optional] max text width (in the same format as fontSize). 
 *   If text width is greater than maxWidth, then font size is scaled down
 * * class - class for text svg element
 *
 * @module utils/chartist/centered-text
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/* global Chartist */

const DEFAULT_FONT_SIZE = '10%';
const DEFAULT_MAX_WIDTH = '40%';

export default function (options) {
  let defaultOptions = {
    text: '',
    fontSize: DEFAULT_FONT_SIZE,
    maxWidth: DEFAULT_MAX_WIDTH,
    class: '',
  };
  options = Chartist.extend({}, defaultOptions, options);
  return (chart) => {
    chart.on('created', () => {
      let svg = chart.svg;
      let relativeSize = Math.min(svg.width(), svg.height());

      let fontSize =
        normalizeSize(options.fontSize, relativeSize, DEFAULT_FONT_SIZE);

      let textAttributes = {
        'dx': svg.width() / 2,
        'dy': svg.height() / 2,
        'text-anchor': 'middle',
        'style': `dominant-baseline: middle; font-size: ${fontSize}px`,
      };

      let textElement = svg.elem('text', textAttributes, 'ct-centered-text')
        .text(options.text);
      let textWidth = textElement.width();
      let maxTextWidth =
        normalizeSize(options.maxWidth, relativeSize, DEFAULT_MAX_WIDTH) - 10;
      if (textWidth > maxTextWidth) {
        let scaleRatio = maxTextWidth / textWidth;
        textElement.getNode().style.fontSize = fontSize * scaleRatio + 'px';
      }
      svg.append(textElement);
    });
  };
}

function normalizeSize(size, relativeSize, defaultValue) {
  if (typeof size === 'string') {
    size = size.trim();
  } else if (typeof size === 'number') {
    return size;
  } else {
    if (typeof defaultValue === 'number') {
      return defaultValue;
    } else {
      size = String(defaultValue);
    }
  }
  if (size[size.length - 1] === '%') {
    return relativeSize * (parseFloat(size) / 100);
  } else {
    return parseFloat(size);
  }
}
