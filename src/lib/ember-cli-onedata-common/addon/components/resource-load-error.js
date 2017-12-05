/**
 * A message to display in place of some resource cannot be loaded. 
 * 
 * NOTE: Backported from `onedata-gui-common`.
 *
 * @module components/resource-load-error
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import layout from 'ember-cli-onedata-common/templates/components/resource-load-error';

import getErrorDetails from 'ember-cli-onedata-common/utils/get-error-description';

const {
  Component,
  computed,
  inject: {
    service,
  },
} = Ember;

export default Component.extend({
  layout,
  classNames: ['alert', 'alert-danger', 'alert-promise-error', 'resource-load-error'],

  i18n: service(),

  /**
   * Action to invoke on alert panel close.
   * If not null - show a close button in alert panel.
   * @type {function|undefined}
   */
  onClose: () => {},

  /**
   * Displayed error details generated from reason error object
   * @type {string}
   */
  _reasonDetails: computed('reason', function () {
    return getErrorDetails(this.get('reason'));
  }),

  init() {
    this._super(...arguments);
    if (!this.get('message')) {
      this.set(
        'message',
        this.get('i18n').t('components.resourceLoadError.defaultErrorMessage')
      );
    }
  },

  actions: {
    toggleShowDetails() {
      this.toggleProperty('showDetails');
    },
    close() {
      this.get('onClose')();
    }
  }
});
