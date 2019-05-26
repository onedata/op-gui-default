/**
 * Common styled spinner for loading more on content lists (eg. files, transfers)
 *
 * @module components/loading-more-spinner
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  computed,
} = Ember;

export default Component.extend({
  classNames: ['spinner-container', 'spinner-centered', 'spinner-64'],
  
  _scale: computed('size', function () {
    return this.get('size') === 'sm' ? 0.2 : 0.4;
  }),
});
