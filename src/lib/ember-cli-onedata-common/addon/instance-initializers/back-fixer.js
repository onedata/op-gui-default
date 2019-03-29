/**
 * Fixes JS evaluation in Firefox and Safari after back button press.
 * 
 * @module instance-initializers/back-fixer
 * @author Jakub Liput, Michał Borzęcki
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import $ from 'jquery';

export function initialize(applicationInstance) {
  // Fix for Firefox
  window.onunload = function () {};

  // Hack to not use Safari cache
  const browser = applicationInstance.lookup('service:browser');
  const isSafari = browser.get('browser.browserCode') === 'safari';
  if (isSafari) {
    // hack to not use Safari cache
    $(window).bind('pagehide', function () {
      setTimeout(() => window.location.reload(), 0);
    });
  }
}

export default {
  initialize: initialize,
};
