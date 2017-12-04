/**
 * FIXME: doc
 * @module components/modals/file-chunks/migrate-popover
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  computed,
} = Ember;

export default Component.extend({
  tagName: 'li',
  classNames: ['migrate-item'],
  classNameBindings: ['providerItemId', 'disabled:disabled:clickable'],
  
  /**
   * @virtual 
   * @type {string}
   */
  providerId: undefined,
  
  /**
   * @virtual 
   * @type {string}
   */
  providerName: undefined,
  
  /**
   * @virtual
   * @type {boolean}
   */
  disabled: false,
  
  /**
   * Will be invoked on click
   * @virtual 
   * @type {function} a providerId is passed as a first arguments
   */
  action: () => {},
    
  click() {
    if (!this.get('disabled')) {
      this.action(this.get('providerId'));
    }
  },
  
  /**
   * Class to pair rendered item with provider
   * @type {Ember.ComputedProperty<string>}
   */
  providerItemId: computed('providerId', function () {
    const providerId = this.get('providerId');
    if (providerId) {
      return `migrate-item-provider-${providerId}`;
    } else {
      return 'migrate-item-provider-no-id';
    }
  }),
});
