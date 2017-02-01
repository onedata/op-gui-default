/**
 * Adds ``animateCss`` method to jQuery objects.
 * @module instance-initializers/animate-css
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export function initialize(/*application*/) {
  $.fn.extend({
    animateCss: function(animationName) {
      var animationEnd = 'webkitAnimationEnd mozAnimationEnd MSAnimationEnd oanimationend animationend';
      this.addClass('animated ' + animationName).one(animationEnd, function() {
          $(this).removeClass('animated ' + animationName);
      });
    }
  });
}

export default {
  initialize,
  name:  'animate-css'
};
