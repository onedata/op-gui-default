import Ember from 'ember';
import PromiseLoadingMixin from '../../mixins/promise-loading';

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

  isLoading: false,

  /**
   * To inject if ``sharePromise`` is not injected
   * @type share
   */
  share: null,

  /**
   * To inject if ``share`` is not injected
   * @type share
   */
  sharePromise: null,

  sharePromiseChanged: Ember.observer('sharePromise', function() {
    const sp = this.get('sharePromise');
    if (sp) {
      this.set('isLoading', true);
      sp.then(
        (share) => {
          this.setProperties({
            share: share,
            sharePromise: null,
          });
        },
        () => {
          this.setProperties({
            share: null,
            sharePromise: null
          });
        }
      );
      sp.finally(
        () => {
          this.set('isLoading', false);
        }
      );
    }
  }),

  init() {
    this._super();
    this.resetProperties();
  },

  resetProperties() {
    this.setProperties({
      share: null,
      sharePromise: null,
      isLoading: false,
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
        this.sendAction('transitionTo', 'shares.show', this.get('share'));
      }
    }
  },

});
