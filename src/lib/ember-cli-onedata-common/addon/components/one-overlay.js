import Ember from 'ember';
import layout from '../templates/components/one-overlay';

const {
  run
} = Ember;

/**
 * An overlay element that covers its parent with yielded content.
 * 
 * Example of usage:
 * ```
 * <div class="main">
 *   {{#if isLoading}}
 *     {{#one-overlay}}
 *       Loading...
 *     {{/one-overlay}}
 *   {{/if}}
 * </div>
 * ```
 * 
 * @module components/one-overlay
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  layout,

  classNames: ['one-overlay'],
  classNameBindings: ['fog:one-overlay-fog'],

  /**
   * To inject.
   * If true, overlay will cover parent with semi-transparent fog
   * with default style (one-overlay-fog).
   * If want to customize fog style, use false and write your own class.
   * @type {Boolean}
   */
  fog: true,

  didInsertElement() {
    this._super(...arguments);
    run.scheduleOnce('afterRender', this, function() {
      let $parent = this.$().parent();
      let parentPosition = $parent.css('position');
      if (parentPosition === 'static') {
        $parent.css('position', 'relative');
      }
      this.set('_origParentPosition', parentPosition);
    });
  },

  willDestroyElement() {
    let _origParentPosition = this.get('_origParentPosition');
    let $parent = this.$().parent();
    $parent.css('position', _origParentPosition);
  }
});
