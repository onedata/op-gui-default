// TODO: common mixin with groups-menu

/**
 * A secondary sidebar for selecting Group to modify its permissions.
 * Renders list of groups-menu-item.
 *
 * @module components/groups-menu
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/

import Ember from 'ember';
import PromiseLoadingMixin from 'ember-cli-onedata-common/mixins/promise-loading';
import ForceReloadCollectionMixin from 'op-worker-gui/mixins/force-reload-collection';

const {
  computed,
  inject,
  observer,
  on,
  get,
  computed: { readOnly },
} = Ember;

export default Ember.Component.extend(PromiseLoadingMixin, ForceReloadCollectionMixin, {
  secondaryMenu: inject.service(),
  store: inject.service(),
  notify: inject.service(),
  oneproviderServer: inject.service(),
  commonModals: inject.service(),
  commonLoader: inject.service(),
  session: inject.service(),

  /** A buffer for token that is entered by user and submitted to backend */
  inputToken: null,

  /** A buffer for Group object that is set before the modal
    (which allows to manipulate a Group) appears. The Group is global
    for all modals, so only one modal should be used at once!
  */
  modalGroup: null,

  groups: null,
  collection: readOnly('groups'),
  
  validGroups: function() {
    return this.get('groups').filter((s) => s.get('isLoaded') && !s.get('isDeleted'));
  }.property('groups', 'groups.[]', 'groups.@each.isLoaded', 'groups.@each.isDeleted'),
  groupsSorting: ['isDefault:desc', 'name'],
  validGroupsSorted: computed.sort('validGroups', 'groupsSorting'),

  activeGroup: computed.alias('secondaryMenu.activeGroup'),

  /**
   * The groups menu is loading if the groups property is null/undefined or
   * any of the groups in group array is not loaded (isLoaded property).
   */
  isLoading: computed('groups.@each.isLoaded', function() {
    let groups = this.get('groups');
    return !groups || groups.any(s => !s || !s.get('isLoaded'));
  }),

  // TODO: what is loading?
  isLoadingChanged: observer('isLoading', function() {
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
  }),

  /*** Variables for actions and modals ***/

  isCreatingGroup: false,
  newGroupName: null,

  isJoiningGroup: false,
  joinGroupToken: null,

  isRenameModalOpened: computed('openedModal', {
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
  isRemoveModalOpened: computed('openedModal', {
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

  isLeaveModalOpened: computed('openedModal', {
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

  isJoinSpaceModalOpened: computed('openedModal', {
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

  isJoinAsSubgroupModalOpened: computed('openedModal', {
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

  isLeaveParentGroupModalOpened: computed('openedModal', {
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

  registerInsecondaryMenu: on('init', function() {
    this.set('secondaryMenu.component', this);
  }),

  activeGroupDidChange: observer('activeGroup', function() {
    let activeGroup = this.get('activeGroup');
    if (activeGroup) {
      this.sendAction('goToGroup', activeGroup);
    }
  }),

  init() {
    this._super(...arguments);
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
      this.setProperties({
        newGroupName: null,
        isSavingGroup: false
      });
    },

    submitCreateGroup() {
      if (this.get('isCreatingGroup')) {
        this.set('isSavingGroup', true);
        this.send('_submitCreateGroup');
      }
    },

    _submitCreateGroup() {
      let name = this.get('newGroupName');
      let user = this.get('session.user');
      let s = this.get('store').createRecord('group', {
        name: name,
        hasViewPrivilege: true,
        user
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
      savePromise.finally(() => {
        this.setProperties({
          isCreatingGroup: false,
          isSavingGroup: false,
        });
      });
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
        this.scheduleReloadCollection();
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
        const p = this.promiseLoading(this.get('oneproviderServer').userLeaveGroup(group.get('id')));
        p.then(
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
        p.finally(() => this.setProperties({
          modalGroup: null,
          openedModel: null,
          isLeaveModalOpened: false
        }));
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
      let {
        inputToken,
        modalGroup,
        oneproviderServer,
        notify,
        i18n
      } = this.getProperties(
        'inputToken',
        'modalGroup',
        'oneproviderServer',
        'notify',
        'i18n'
      );
      let token = inputToken && inputToken.trim();
      let group = modalGroup;
      let promise = this.promiseLoading(oneproviderServer.groupJoinSpace(modalGroup.get('id'), token))
        .then(
          (data) => {
            let message = i18n.t('components.groupsMenu.notify.joinSpaceSuccess', {
              groupName: group.get('name'),
              spaceName: data.spaceName
            });
            notify.info(message);
          },
          (error) => {
            console.log(error.message);
            let message = i18n.t('components.groupsMenu.notify.joinSpaceFailed', {
              groupName: group.get('name'),
            });
            message = message + ': ' + error.message;
            notify.error(message);
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
      let {
        inputToken,
        modalGroup: group,
        oneproviderServer,
        i18n,
        reloadCollectionTimeout,
        notify,
      } = this.getProperties(
        'inputToken',
        'modalGroup',
        'oneproviderServer',
        'i18n',
        'reloadCollectionTimeout',
        'notify'
      );
      let token = inputToken && inputToken.trim();
      let promise = this.promiseLoading(
        oneproviderServer.groupJoinGroup(get(group, 'id'), token)
      ).then(
          (data) => {
            let message = i18n.t('components.groupsMenu.notify.joinAsSubgroupSuccess', {
              thisGroupName: get(group, 'name'),
              groupName: data.groupName
            });
            notify.info(message);
          },
          (error) => {
            console.log(error.message);
            let message = i18n.t('components.groupsMenu.notify.joinAsSubgroupFailed', {
              groupName: get(group, 'name'),
            });
            message = message + ': ' + error.message;
            notify.error(message);
          }
      );
      promise.finally(() => {
        this.scheduleReloadCollection();
        setTimeout(() => group.reload(), reloadCollectionTimeout);
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
