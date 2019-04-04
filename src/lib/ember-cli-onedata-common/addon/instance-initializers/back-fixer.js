/**
 * Fixes JS evaluation in Firefox and Safari after back button press.
 * 
 * @module instance-initializers/back-fixer
 * @author Jakub Liput, Michał Borzęcki
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import $ from 'jquery';
import Ember from 'ember';

const {
  run: { next },
} = Ember;

export function initialize(applicationInstance) {
  // Fix for Firefox
  window.addEventListener('unload', function () {});

  // Hack to not use Safari cache
  const browser = applicationInstance.lookup('service:browser');
  const isSafari = browser.get('browser.browserCode') === 'safari';
  if (isSafari) {
    $(window).bind('pagehide', function () {
      window.onedataIsReloadingApp = 1;
      next(() => window.location.reload());
    });
  }
}

export default {
  initialize: initialize,
};
