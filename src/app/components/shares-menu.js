// TODO: common mixin with shares-menu

/**
 * A secondary sidebar for selecting Share to modify its permissions.
 * Renders list of shares-menu-item.
 *
 * @module components/shares-menu
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/

import Ember from 'ember';
import PromiseLoadingMixin from 'op-worker-gui/mixins/promise-loading';

export default Ember.Component.extend(PromiseLoadingMixin, {
  secondaryMenu: Ember.inject.service(),
  store: Ember.inject.service(),
  notify: Ember.inject.service(),
  oneproviderServer: Ember.inject.service(),
  commonModals: Ember.inject.service(),
  commonLoader: Ember.inject.service(),

  /** A buffer for token that is entered by user and submitted to backend */
  inputToken: null,

  /** A buffer for Share object that is set before the modal
    (which allows to manipulate a Share) appears. The Share is global
    for all modals, so only one modal should be used at once!
  */
  modalShare: null,

  shares: null,
  validShares: function() {
    return this.get('shares').filter((s) => s.get('isLoaded'));
  }.property('shares', 'shares.[]', 'shares.@each.isLoaded'),
  sharesSorting: ['isDefault:desc', 'name'],
  validSharesSorted: Ember.computed.sort('validShares', 'sharesSorting'),

  activeShare: Ember.computed.alias('secondaryMenu.activeShare'),

  /**
   * The shares menu is loading if the shares property is null/undefined or
   * any of the shares in share array is not loaded (isLoaded property).
   */
  isLoading: function() {
    return !this.get('shares') || this.get('shares').any((s) => !s.get('isLoaded'));
  }.property('shares', 'shares.@each.isLoaded'),

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

  isCreatingShare: false,
  newShareName: null,

  isJoiningShare: false,
  joinShareToken: null,

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

  isLeaveModalOpened: Ember.computed('openedModal', {
    get() {
      return this.get('openedModal') === 'leave';
    },
    set(key, value) {
      if (!value) {
        this.set('openedModal', null);
      }
      return value;
    }
  }),

  isJoinSpaceModalOpened: Ember.computed('openedModal', {
    get() {
      return this.get('openedModal') === 'joinSpace';
    },
    set(key, value) {
      if (!value) {
        this.set('openedModal', null);
      }
      return value;
    }
  }),

  isJoinAsSubshareModalOpened: Ember.computed('openedModal', {
    get() {
      return this.get('openedModal') === 'joinAsSubshare';
    },
    set(key, value) {
      if (!value) {
        this.set('openedModal', null);
      }
      return value;
    }
  }),

  isLeaveParentShareModalOpened: Ember.computed('openedModal', {
    get() {
      return this.get('openedModal') === 'leaveParentShare';
    },
    set(key, value) {
      if (!value) {
        this.set('openedModal', null);
      }
      return value;
    }
  }),

  registerInsecondaryMenu: function() {
    this.set('secondaryMenu.component', this);
  }.on('init'),

  activeShareDidChange: function() {
    if (this.get('activeShare')) {

      this.sendAction('goToShare', this.get('activeShare'));
    }
  }.observes('activeShare'),

  didInsertElement() {
    // reset shares expanded state
    this.get('shares').forEach((s) => s.set('isExpanded', false));

    this.isLoadingChanged();
  },

  shareActionMessage(notifyType, messageId, shareName) {
    let message = this.get('i18n').t(`components.sharesMenu.notify.${messageId}`, {shareName: shareName});
    this.get('notify')[notifyType](message);
  },

  actions: {
    openSubmenuEntry(share, name) {
      console.debug(`shares-menu: openSubmenuEntry(${share}, ${name})`);
      this.sendAction('openSubmenuEntry', share, name);
    },

    openSettingsModal(modalName, share) {
      this.set('modalShare', share);
      this.set('openedModal', modalName);
    },

    startCreateShare() {
      this.set('isCreatingShare', true);
    },

    createShareModalOpened() {
      this.setProperties({
        newShareName: null,
        isSavingShare: false
      });
    },

    submitCreateShare() {
      if (this.get('isCreatingShare')) {
        this.set('isSavingShare', true);
        this.send('_submitCreateShare');
      }
    },

    _submitCreateShare() {
      let name = this.get('newShareName');
      let s = this.get('store').createRecord('share', {
        name: name
      });
      let savePromise = s.save();
      savePromise.then(
        () => {
          this.get('i18n').t('components.sharesMenu.notify.createSuccess', {
            name: name
          });
        },
        (error) => {
          this.get('notify').error(
            this.get('i18n').t('components.sharesMenu.notify.createFailed', {
              name: name
            }) + ': ' + ((error && error.message) || this.get('i18n').t('common.unknownError'))
          );
          s.deleteRecord();
        }
      );
      savePromise.finally(() => {
        this.setProperties({
          isCreatingShare: false,
          isSavingShare: false,
        });
        console.error('finally saved');
      });
    },

    startJoinShare() {
      this.setProperties({
        inputToken: null,
        isJoiningShare: true
      });
    },

    submitJoinShare() {
      this.set('isJoiningShareWorking', true);
      let token = this.get('inputToken') && this.get('inputToken').trim();
      let serverPromise = this.get('oneproviderServer').userJoinShare(token);
      serverPromise.then(
        (data) => {
          this.shareActionMessage('info', 'joinSuccess', data.shareName);
        },
        (errorJson) => {
          console.log(errorJson.message);
          let message = this.get('i18n').t('components.sharesMenu.notify.joinFailed', {errorDetails: errorJson.message});
          this.get('notify').error(message);
        }
      );
      serverPromise.finally(() => {
        this.setProperties({
          inputToken: null,
          isJoiningShareWorking: false,
          isJoiningShare: false
        });
      });
    },

    submitLeaveShare() {
      try {
        let share = this.get('modalShare');
        let shareName = share.get('name');
        const p = this.promiseLoading(this.get('oneproviderServer').userLeaveShare(share.get('id')));
        p.then(
          () => {
            share.deleteRecord();
            let message = this.get('i18n').t('components.sharesMenu.notify.leaveSuccess', {
              name: shareName
            });
            this.get('notify').info(message);
            if (share.get('id') === this.get('activeShare.id')) {
              this.sendAction('goToShare', null);
            }
          },
          (error) => {
            console.log(`Leave share ${shareName} failed ${error.message}`);
            let message = this.get('i18n').t('components.sharesMenu.notify.leaveFailed', {
              name: shareName
            });
            message = message + ': ' + error.message;
            this.get('notify').error(message);
          }
        );
        p.finally(() => this.setProperties({
          modalShare: null,
          openedModel: null,
          isLeaveModalOpened: false
        }));
      } finally {
        this.setProperties({
          modalShare: null,
          openedModel: null
        });
      }
    },

    startJoinSpace() {
      this.set('inputToken', null);
    },

    submitJoinSpace() {
      let token = this.get('inputToken') && this.get('inputToken').trim();
      let share = this.get('modalShare');
      let promise = this.promiseLoading(this.get('oneproviderServer')
        .shareJoinSpace(this.get('modalShare.id'), token)).then(
          (data) => {
            let message = this.get('i18n').t('components.sharesMenu.notify.joinSpaceSuccess', {
              shareName: share.get('name'),
              spaceName: data.spaceName
            });
            this.get('notify').info(message);
          },
          (error) => {
            console.log(error.message);
            let message = this.get('i18n').t('components.sharesMenu.notify.joinSpaceFailed', {
              shareName: share.get('name'),
            });
            message = message + ': ' + error.message;
            this.get('notify').error(message);
          }
      );
      promise.finally(() => {
        this.setProperties({
          inputToken: null,
          isJoiningSpaceWorking: false,
          isJoinSpaceModalOpened: false
        });
      });
    },

    startJoinAsSubshare() {
      this.set('inputToken', null);
    },

    submitJoinAsSubshare() {
      let token = this.get('inputToken') && this.get('inputToken').trim();
      let share = this.get('modalShare');
      let promise = this.promiseLoading(this.get('oneproviderServer')
        .shareJoinShare(this.get('modalShare.id'), token)).then(
          (data) => {
            let message = this.get('i18n').t('components.sharesMenu.notify.joinAsSubshareSuccess', {
              thisShareName: share.get('name'),
              shareName: data.shareName
            });
            this.get('notify').info(message);
          },
          (error) => {
            console.log(error.message);
            let message = this.get('i18n').t('components.sharesMenu.notify.joinAsSubshareFailed', {
              shareName: share.get('name'),
            });
            message = message + ': ' + error.message;
            this.get('notify').error(message);
          }
      );
      promise.finally(() => {
        this.setProperties({
          inputToken: null,
          isJoiningAsSubshareWorking: false,
          isJoinAsSubshareModalOpened: false
        });
      });
    },

    startLeaveParentShare() {
      this.set('parentShareToLeave', this.get('modalShare.parentShares').objectAt(0));
    },

    submitLeaveParentShare() {
      try {
        let parentShare = this.get('parentShareToLeave');
        let subshare = this.get('modalShare');
        this.promiseLoading(
          this.get('oneproviderServer').shareLeaveShare(parentShare.get('id'), subshare.get('id')),
          () => this.set('isLeaveParentShareWorking', true),
          () => this.set('isLeaveParentShareWorking', false)
        ).then(
          () => {
            let message = this.get('i18n').t('components.sharesMenu.notify.leaveParentShareSuccess', {
              subshareName: subshare.get('name'),
              parentShareName: parentShare.get('name')

            });
            this.get('notify').info(message);
          },
          (error) => {
            let message = this.get('i18n').t('components.sharesMenu.notify.leaveParentShareFailed', {
              subshareName: subshare.get('name'),
              parentShareName: parentShare.get('name')
            });
            message = message + ': ' + error.message;
            this.get('notify').error(message);
          }
        );
      } finally {
        this.setProperties({
          modelShare: null,
          openedModal: null,
          parentShareToLeave: null,
        });
      }
    },
  },

});
