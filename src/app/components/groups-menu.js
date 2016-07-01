// TODO: common mixin with groups-menu

/**
 * A secondary sidebar for selecting Group to modify its permissions.
 * Renders list of groups-menu-item.
 *
 * @module components/groups-menu
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/

import Ember from 'ember';
import PromiseLoadingMixin from '../mixins/promise-loading';

export default Ember.Component.extend(PromiseLoadingMixin, {
  secondaryMenu: Ember.inject.service(),
  store: Ember.inject.service(),
  notify: Ember.inject.service(),
  oneproviderServer: Ember.inject.service(),
  commonModals: Ember.inject.service(),
  commonLoader: Ember.inject.service(),

  /** A buffer for token that is entered by user and submitted to backend */
  inputToken: null,

  /** A buffer for Group object that is set before the modal
    (which allows to manipulate a Group) appears. The Group is global
    for all modals, so only one modal should be used at once!
  */
  modalGroup: null,

  groups: null,
  validGroups: function() {
    return this.get('groups').filter((s) => s.get('isLoaded'));
  }.property('groups', 'groups.[]', 'groups.@each.isLoaded'),
  groupsSorting: ['isDefault:desc', 'name'],
  validGroupsSorted: Ember.computed.sort('validGroups', 'groupsSorting'),

  activeGroup: Ember.computed.alias('secondaryMenu.activeGroup'),

  /**
   * The groups menu is loading if the groups property is null/undefined or
   * any of the groups in group array is not loaded (isLoaded property).
   */
  isLoading: function() {
    return !this.get('groups') || this.get('groups').any((s) => !s.get('isLoaded'));
  }.property('groups', 'groups.@each.isLoaded'),

  // TODO: what is loading?
  isLoadingChanged: function() {
    if (this.get('isLoading')) {
      this.setProperties({
        'commonLoader.isLoading': true,
        'commonLoader.message': this.get('i18n').t('components.commonLoader.synchronizingGroups'),
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

  isCreatingGroup: false,
  newGroupName: null,

  isJoiningGroup: false,
  joinGroupToken: null,

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

  groupToRemove: null,
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

  isJoinAsSubgroupModalOpened: Ember.computed('openedModal', {
    get() {
      return this.get('openedModal') === 'joinAsSubgroup';
    },
    set(key, value) {
      if (!value) {
        this.set('openedModal', null);
      }
      return value;
    }
  }),

  isLeaveParentGroupModalOpened: Ember.computed('openedModal', {
    get() {
      return this.get('openedModal') === 'leaveParentGroup';
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

  activeGroupDidChange: function() {
    if (this.get('activeGroup')) {

      this.sendAction('goToGroup', this.get('activeGroup'));
    }
  }.observes('activeGroup'),

  didInsertElement() {
    // reset groups expanded state
    this.get('groups').forEach((s) => s.set('isExpanded', false));

    this.isLoadingChanged();
  },

  groupActionMessage(notifyType, messageId, groupName) {
    let message = this.get('i18n').t(`components.groupsMenu.notify.${messageId}`, {groupName: groupName});
    this.get('notify')[notifyType](message);
  },

  actions: {
    openSubmenuEntry(group, name) {
      console.debug(`groups-menu: openSubmenuEntry(${group}, ${name})`);
      this.sendAction('openSubmenuEntry', group, name);
    },

    openSettingsModal(modalName, group) {
      this.set('modalGroup', group);
      this.set('openedModal', modalName);
    },

    startCreateGroup() {
      this.set('isCreatingGroup', true);
    },

    createGroupModalOpened() {
      this.set('newGroupName', null);
    },

    submitCreateGroup() {
      // isSaving flag is set by spin-button on click
      let name = this.get('newGroupName');
      let s = this.get('store').createRecord('group', {
        name: name
      });
      let savePromise = s.save();
      savePromise.then(
        () => {
          this.get('i18n').t('components.groupsMenu.notify.createSuccess', {
            name: name
          });
        },
        (error) => {
          this.get('notify').error(
            this.get('i18n').t('components.groupsMenu.notify.createFailed', {
              name: name
            }) + ': ' + ((error && error.message) || this.get('i18n').t('common.unknownError'))
          );
          s.deleteRecord();
        }
      );
      savePromise.finally(() => this.setProperties({
        isCreatingGroup: false,
        isSavingGroup: false
      }));
    },

    startJoinGroup() {
      this.setProperties({
        inputToken: null,
        isJoiningGroup: true
      });
    },

    submitJoinGroup() {
      this.set('isJoiningGroupWorking', true);
      let token = this.get('inputToken') && this.get('inputToken').trim();
      let serverPromise = this.get('oneproviderServer').userJoinGroup(token);
      serverPromise.then(
        (data) => {
          this.groupActionMessage('info', 'joinSuccess', data.groupName);
        },
        (errorJson) => {
          console.log(errorJson.message);
          let message = this.get('i18n').t('components.groupsMenu.notify.joinFailed', {errorDetails: errorJson.message});
          this.get('notify').error(message);
        }
      );
      serverPromise.finally(() => {
        this.setProperties({
          inputToken: null,
          isJoiningGroupWorking: false,
          isJoiningGroup: false
        });
      });
    },

    submitLeaveGroup() {
      try {
        let group = this.get('modalGroup');
        let groupName = group.get('name');
        this.promiseLoading(this.get('oneproviderServer').userLeaveGroup(group.get('id')))
          .then(
            () => {
              group.deleteRecord();
              let message = this.get('i18n').t('components.groupsMenu.notify.leaveSuccess', {
                name: groupName
              });
              this.get('notify').info(message);
              if (group.get('id') === this.get('activeGroup.id')) {
                this.sendAction('goToGroup', null);
              }
            },
            (error) => {
              console.log(`Leave group ${groupName} failed ${error.message}`);
              let message = this.get('i18n').t('components.groupsMenu.notify.leaveFailed', {
                name: groupName
              });
              message = message + ': ' + error.message;
              this.get('notify').error(message);
            }
        );
      } finally {
        this.setProperties({
          modalGroup: null,
          openedModel: null
        });
      }
    },

    startJoinSpace() {
      this.set('inputToken', null);
    },

    submitJoinSpace() {
      let token = this.get('inputToken') && this.get('inputToken').trim();
      let group = this.get('modalGroup');
      let promise = this.promiseLoading(this.get('oneproviderServer')
        .groupJoinSpace(this.get('modalGroup.id'), token)).then(
          (data) => {
            let message = this.get('i18n').t('components.groupsMenu.notify.joinSpaceSuccess', {
              groupName: group.get('name'),
              spaceName: data.spaceName
            });
            this.get('notify').info(message);
          },
          (error) => {
            console.log(error.message);
            let message = this.get('i18n').t('components.groupsMenu.notify.joinSpaceFailed', {
              groupName: group.get('name'),
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

    startJoinAsSubgroup() {
      this.set('inputToken', null);
    },

    submitJoinAsSubgroup() {
      let token = this.get('inputToken') && this.get('inputToken').trim();
      let group = this.get('modalGroup');
      let promise = this.promiseLoading(this.get('oneproviderServer')
        .groupJoinGroup(this.get('modalGroup.id'), token)).then(
          (data) => {
            let message = this.get('i18n').t('components.groupsMenu.notify.joinAsSubgroupSuccess', {
              thisGroupName: group.get('name'),
              groupName: data.groupName
            });
            this.get('notify').info(message);
          },
          (error) => {
            console.log(error.message);
            let message = this.get('i18n').t('components.groupsMenu.notify.joinAsSubgroupFailed', {
              groupName: group.get('name'),
            });
            message = message + ': ' + error.message;
            this.get('notify').error(message);
          }
      );
      promise.finally(() => {
        this.setProperties({
          inputToken: null,
          isJoiningAsSubgroupWorking: false,
          isJoinAsSubgroupModalOpened: false
        });
      });
    },

    startLeaveParentGroup() {
      this.set('parentGroupToLeave', this.get('modalGroup.parentGroups').objectAt(0));
    },

    submitLeaveParentGroup() {
      try {
        let parentGroup = this.get('parentGroupToLeave');
        let subgroup = this.get('modalGroup');
        this.promiseLoading(
          this.get('oneproviderServer').groupLeaveGroup(parentGroup.get('id'), subgroup.get('id')),
          () => this.set('isLeaveParentGroupWorking', true),
          () => this.set('isLeaveParentGroupWorking', false)
        ).then(
          () => {
            let message = this.get('i18n').t('components.groupsMenu.notify.leaveParentGroupSuccess', {
              subgroupName: subgroup.get('name'),
              parentGroupName: parentGroup.get('name')

            });
            this.get('notify').info(message);
          },
          (error) => {
            let message = this.get('i18n').t('components.groupsMenu.notify.leaveParentGroupFailed', {
              subgroupName: subgroup.get('name'),
              parentGroupName: parentGroup.get('name')
            });
            message = message + ': ' + error.message;
            this.get('notify').error(message);
          }
        );
      } finally {
        this.setProperties({
          modelGroup: null,
          openedModal: null,
          parentGroupToLeave: null,
        });
      }
    },
  },

});
