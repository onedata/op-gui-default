import Ember from 'ember';
import PromiseLoadingMixin from 'op-worker-gui/mixins/promise-loading';

/**
 * Modal that shows summary (basic information) about Share.
 * An instance of Share should be provided.
 *
 * @module modals/share-info
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend(PromiseLoadingMixin, {
  store: Ember.inject.service(),
  i18n: Ember.inject.service(),
  oneproviderServer: Ember.inject.service(),

  /** @abstract */
  modalId: null,

  /** @abstract */
  label: null,

  open: false,

  error: null,

  /**
   * @type Share or Ember.ObjectProxy of Share
   */
  share: null,

  isLoading: Ember.computed('share.content', function() {
    return !this.get('share.content');
  }),

  init() {
    this._super();
    this.resetProperties();
  },

  resetProperties() {
    this.setProperties({
      share: null
    });
  },

  goToShareDisabled: Ember.computed('isLoading', 'share', function() {
    return this.get('isLoading') || !this.get('share');
  }),

  actions: {
    open() {
    },

    opened() {
    },

    closed() {
      this.resetProperties();
    },

    goToShare() {
      const s = this.get('share');
      if (s) {
        this.set('open', false);
        this.sendAction('transitionTo', 'onedata.shares.show', this.get('share'));
      }
    },
  },

});
