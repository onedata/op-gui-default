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
  Component
} = Ember;

export default Component.extend({
  classNames: ['spinner-container', 'spinner-centered', 'spinner-64'],
});
