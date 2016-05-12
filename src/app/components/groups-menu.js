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

  groups: null,
  validGroups: function() {
    return this.get('groups').filter((s) => s.get('isLoaded'));
  }.property('groups', 'groups.[]', 'groups.@each.isLoaded'),
  groupsSorting: ['isDefault:desc', 'name'],
  validGroupsSorted: Ember.computed.sort('validGroups', 'groupsSorting'),

  activeGroup: Ember.computed.alias('secondaryMenu.activeItem'),

  isLoading: function() {
    return !this.get('groups.length') || this.get('groups').any((s) => !s.get('name'));
  }.property('groups', 'groups.length', 'groups.@each.name'),

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

  groupToRename: null,
  renameGroupName: null,
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

  registerInsecondaryMenu: function() {
    this.set('secondaryMenu.component', this);
  }.on('init'),

  clearsecondaryMenu: function() {
    this.get('secondaryMenu').clear();
  }.on('willDestroyElement'),

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
      this.set('joinGroupToken', null);
      this.set('isJoiningGroup', true);
    },

    submitJoinGroup() {
      let token = this.get('joinGroupToken') && this.get('joinGroupToken').trim();
      let serverPromise = this.get('oneproviderServer').userJoinGroup(token);
      serverPromise.then(
        (groupName) => {
          this.groupActionMessage('info', 'joinSuccess', groupName);
        },
        (errorJson) => {
          console.log(errorJson.message);
          let message = this.get('i18n').t('components.groupsMenu.notify.joinFailed', {errorDetails: errorJson.message});
          this.get('notify').error(message);
        }
      );
      serverPromise.finally(() => {
        this.set('isJoiningGroupWorking', false);
        this.set('isJoiningGroup', false);
      });
    },

    submitRenameGroup() {
      try {
        let group = this.get('modalGroup');
        let oldName = group.get('name');
        let newName = this.get('renameGroupName');
        group.set('name', this.get('renameGroupName'));

        this.promiseLoading(
          group.save()
        ).then(
          () => {
            this.get('notify').info(this.get('i18n').t(
              'components.groupsMenu.notify.renameSuccess', {
                oldName: oldName,
                newName: newName
              }
            ));
          },
          (error) => {
            this.get('notify').error(this.get('i18n').t(
              'components.groupsMenu.notify.renameFailed', {
                oldName: oldName,
                newName: newName
              }
            ) + ': ' + error.message);
            group.rollbackAttributes();
          }
        );
      } finally {
        this.setProperties({
          modalGroup: null,
          openedModal: null
        });
      }
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
        this.set('modalGroup', null);
        this.set('openedModal', null);
      }
    },

    startJoinSpace() {
      this.set('joinSpaceToken', null);
      this.set('isJoiningSpace', true);
    },

    submitJoinSpace() {
      let token = this.get('joinSpaceToken') && this.get('joinSpaceToken').trim();
      let serverPromise = this.get('oneproviderServer').userJoinSpace(token);
      serverPromise.then(
        () => {
          // TODO FIXME
        },
        (errorJson) => {
          console.log(errorJson.message);
          let message = this.get('i18n').t('components.groupSettingsDrop.notify.joinSpaceFailed', {errorDetails: errorJson.message});
          this.get('notify').error(message);
        }
      );
      serverPromise.finally(() => {
        this.set('isJoiningGroupWorking', false);
        this.set('isJoiningGroup', false);
      });
    },
  }
});
