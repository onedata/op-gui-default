/**
 * Inline element for showing error (eg. small portion of data cannot be loaded)
 *
 * @module components/error-inline
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import layout from '../templates/components/error-inline';

const {
  Component,
  computed,
  inject: { service },
} = Ember;

export default Component.extend({
  layout,
  tagName: 'span',
  classNames: ['error-inline'],
  
  i18n: service(),
  
  /**
   * Hint shown on hover
   * @type {string}
   */
  hint: computed('i18n', function () {
    return this.get('i18n').t('components.errorInline.defaultHint');
  }),
});
