// TODO: common mixin with shares-menu

/**
 * A secondary sidebar for selecting Share show its view.
 * Renders list of shares-menu-item.
 *
 * @module components/shares-menu
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/

import Ember from 'ember';
import PromiseLoadingMixin from 'ember-cli-onedata-common/mixins/promise-loading';

export default Ember.Component.extend(PromiseLoadingMixin, {
  secondaryMenu: Ember.inject.service(),
  store: Ember.inject.service(),
  notify: Ember.inject.service(),
  oneproviderServer: Ember.inject.service(),
  commonModals: Ember.inject.service(),
  commonLoader: Ember.inject.service(),

  /** A buffer for Share object that is set before the modal
    (which allows to manipulate a Share) appears. The Share is global
    for all modals, so only one modal should be used at once!
  */
  modalShare: null,

  shares: null,
  validShares: function() {
    return this.get('shares').filter((s) => s.get('isLoaded') && !s.get('isDeleted'));
  }.property('shares', 'shares.[]', 'shares.@each.isLoaded', 'shares.@each.isDeleted'),
  sharesSorting: ['name'],
  validSharesSorted: Ember.computed.sort('validShares', 'sharesSorting'),

  activeShare: Ember.computed.alias('secondaryMenu.activeShare'),

  /**
   * The shares menu is loading if the shares property is null/undefined or
   * any of the shares in share array is not loaded (isLoaded property).
   */
  isLoading: function() {
    return !this.get('shares') || this.get('shares').any((s) => !s.get('isLoaded'));
  }.property('shares', 'shares.@each.isLoaded'),

  // TODO: make a mixin for all *-menus components with this method
  // TODO: what is loading?
  isLoadingChanged: function() {
    if (this.get('isLoading')) {
      this.setProperties({
        'commonLoader.isLoading': true,
        'commonLoader.message': this.get('i18n').t('components.commonLoader.synchronizingShares'),
        'commonLoader.messageSecondary': this.get('i18n').t('components.commonLoader.firstLogin')
      });
    } else {
      this.setProperties({
        'commonLoader.isLoading': false,
        'commonLoader.message': null,
        'commonLoader.messageSecondary': null,
      });
    }
  }.observes('isLoading'),

  /*** Variables for actions and modals ***/

  // TODO: thes properties should be created dynamically using name of modal

  isRenameModalOpened: Ember.computed('openedModal', {
    get() {
      return this.get('openedModal') === 'rename';
    },
    set(key, value) {
      if (!value) {
        this.set('openedModal', null);
      }
      return value;
    }
  }),

  shareToRemove: null,
  isRemoveModalOpened: Ember.computed('openedModal', {
    get() {
      return this.get('openedModal') === 'remove';
    },
    set(key, value) {
      if (!value) {
        this.set('openedModal', null);
      }
      return value;
    }
  }),

  // TODO: move to *-menu mixin

  registerInsecondaryMenu: function() {
    this.set('secondaryMenu.component', this);
  }.on('init'),

  // TODO: move to *-menu mixin

  activeShareDidChange: function() {
    if (this.get('activeShare')) {

      this.sendAction('goToShare', this.get('activeShare'));
    }
  }.observes('activeShare'),

  init() {
    this._super(...arguments);
    this.isLoadingChanged();
  },

  shareActionMessage(notifyType, messageId, shareName) {
    let message = this.get('i18n').t(`components.sharesMenu.notify.${messageId}`, {shareName: shareName});
    this.get('notify')[notifyType](message);
  },

  actions: {
    // TODO: this action can be defined in mixin for all *-menu
    openSettingsModal(modalName, share) {
      this.set('modalShare', share);
      this.set('openedModal', modalName);
    },

    /**
     * Handle Yes/No answer of remove share modal.
     * @param {Boolean} yesAnswer if user answered Yes to remove a share
     * @param {Share} share a share which question was about
     */
    handleRemoveAnswer(yesAnswer, share, resolve, reject) {
      let shareName = share.get('name');
      let destroyPromise = share.destroyRecord();

      destroyPromise.then(() => {
        resolve();
        if (share === this.get('activeShare')) {
          this.sendAction('transitionTo', 'onedata.shares.index');
        }
        this.get('notify').info(this.get('i18n').t(
          'components.modals.removeModal.removeSuccess', {
            name: shareName
          }
        ));
      });

      destroyPromise.catch(error => {
        reject();
        this.get('notify').error(this.get('i18n').t(
          'components.modals.removeModal.removeFailed', {
            name: shareName
          }
        ) + ': ' + error.message);
        share.rollbackAttributes();
      });
    },
  },

});
