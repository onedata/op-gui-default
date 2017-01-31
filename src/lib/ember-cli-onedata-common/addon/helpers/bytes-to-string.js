import Ember from 'ember';

import bytesToStringUtil from 'ember-cli-onedata-common/utils/bytes-to-string';

/**
 * A helper to use bytes-to-string util.
 *
 * @module helpers/bytes-to-string
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export function bytesToString([bytes], options) {
  return bytesToStringUtil(bytes, options);
}

export default Ember.Helper.helper(bytesToString);
