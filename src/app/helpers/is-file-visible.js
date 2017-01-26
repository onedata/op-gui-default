import Ember from 'ember';

/**
 * A test to check if File should be presented in file browser.
 * @module helpers/is-file-visible
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export function isFileVisible([file]/*, hash*/) {
  return file.get('isLoaded') && file.get('isBroken') === false;
}

export default Ember.Helper.helper(isFileVisible);
