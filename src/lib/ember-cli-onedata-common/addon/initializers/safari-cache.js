/**
 * Force reload app after using back/forward button in Safari
 * @module initializers/safari-cache
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export function initialize() {
  $(window).bind('pagehide', function() {
    setTimeout(() => {
      const currentLocation = window.location;
      let newLocation = currentLocation;
      if (!/.*#.*\?.*/.test(currentLocation)) {
        newLocation += '?';
      }
      newLocation += '&back_forward=true';
      window.location = newLocation;
      window.location.reload();
    }, 0);
  });
}

export default {
  name: 'safari-cache',
  initialize: initialize
};
