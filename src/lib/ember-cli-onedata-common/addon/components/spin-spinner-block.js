/**
 * A container with spin-spinner.
 * 
 * NOTE: Backported from `onedata-gui-common`. 
 * 
 * Facilitates positioning and setting size of spinner.
 * 
 * @module components/spin-spinner-block
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import layout from 'ember-cli-onedata-common/templates/components/spin-spinner-block';

const PREDEF_SIZES = {
  xxs: 0.12,
  xs: 0.2,
  sm: 0.4,
  md: 0.8,
  lg: 1.2
};

const {
  computed
} = Ember;

export default Ember.Component.extend({
  layout,
  tagName: 'div',
  classNames: ['spin-spinner-block', 'spinner-container'],
  classNameBindings: ['sizeClass'],

  sizeClass: 'lg',

  spinnerScale: computed('sizeClass', function () {
    return PREDEF_SIZES[this.get('sizeClass')];
  })
});
