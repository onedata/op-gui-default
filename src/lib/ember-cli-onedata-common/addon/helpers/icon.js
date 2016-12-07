import Ember from 'ember';

// TODO: refactor OP to use one-icon component from deps/gui
/**
 * Insert an icon from onedata icons collection.
 *
 * @param {string} name - icon name in collection (see icon-* style classes)
 * @param {string} classes - additional classes, eg. for color (see color-* style classes)
 *
 * @returns {string} an icon span
 * @module helpers/icon
 * @todo maybe it should be a Component
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/

/**
 * Returns a HTML code that will insert a Onedata icon
 *
 * @param {String[]} params - arguments for helper
 * @param {String} params[0] - name of an icon
 * @param {String} params[1] - classes string
 * @returns {Ember.String.htmlSafe} a HTML code
 */
export function iconHTML(params) {
  let name = params[0];
  let classes = params[1] || '';

  var html = '<span class="oneicon oneicon-'+name+' '+classes+'"></span>';
  return new Ember.String.htmlSafe(html);
}

export default Ember.Helper.helper(iconHTML);
