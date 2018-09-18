/**
 * Force reload app after using back/forward button in Safari with special flag
 * indicating, that used back/forward in browser.
 * 
 * @module instance-initializers/back-forward
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export function initialize(applicationInstance) {
  const browser = applicationInstance.lookup('service:browser');
  const isSafari = browser.get('browser.browserCode') === 'safari';
  if (checkBackForward()) {
    setBackForwardFlag(isSafari);
  }
  $(window).bind('pagehide', function() {
    setTimeout(() => setBackForwardFlag(isSafari), 0);
  });
}

function checkBackForward() {
  if (performance && performance.navigation) {
    // deprecated API
    return performance && performance.navigation.type === performance.navigation.TYPE_BACK_FORWARD;
  } else {
    // new experimental API
    return performance.getEntriesByType('navigation')[0].type === 'back_forward';
  }
  
}

function setBackForwardFlag(isSafari) {
  const currentLocation = window.location;
  let newLocation = currentLocation;
  newLocation += /.*#.*\?.*/.test(currentLocation) ? '&' : '?';
  newLocation += 'back_forward=true';
  window.location.replace(newLocation);
  if (isSafari) {
    // hack to not use Safari cache
    window.location.reload();
  }
}


export default {
  initialize: initialize,
};
