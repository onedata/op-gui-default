/**
 * Plugin for Chartist which changes chart elements styles to custom values 
 * using data.customCss. It has to be a list with objects in format e.g.:
 * {
 *   slice: {
 *      'color': 'white',
 *   }
 * }
 * Name of an element is the same as the one we can obtain from data.type,
 * where `data` if from `chart.on('draw', (data) => {...})`. Styles are applied to
 * `data.element`.
 * 
 * If transition is necessary, special object transitionProperties 
 * can be used. It is a object with css properties and values, 
 * that will be applied just after (in the next run-loop) standard 
 * (not in transitionProperties) properties application. Example:
 * {
 *   slice: {
 *     'stroke-opacity': '0.5',
 *     transitionProperties: {
 *       'transition': 'stroke-opacity 0.4s',
 *       'stroke-opacity: '1',
 *     }
 *   }
 * }
 * Here first `'stroke-opacity': '0.5'` will be applied and then, in the next
 * run-loop, all properties from transitionProperties. In result we will see
 * stroke-opacity animation 0.5 to 1.
 *
 * @module utils/chartist/custom-css
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

export default function () {
  return (chart) => {
    chart.on('draw', (data) => {
      let css = chart.data.customCss;
      let elementCss = css && css[data.index] && css[data.index][data.type];
      if (elementCss) {
        let element = $(data.element.getNode());
        let transitionProperties = elementCss.transitionProperties;
        delete elementCss.transitionProperties;
        element.css(elementCss);
        if (transitionProperties) {
          setTimeout(() => element.css(transitionProperties), 0);
        }
      }
    });
  };
}
