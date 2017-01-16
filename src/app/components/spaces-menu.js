/**
 * A secondary sidebar for selecting Space to modify its permissions.
 * Renders list of spaces-menu-item.
 *
 * @module components/spaces-menu
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/

import Ember from 'ember';

const {
  observer,
  computed,
  inject
} = Ember;

export default Ember.Component.extend({
  secondaryMenu: inject.service(),
  store: inject.service(),
  notify: inject.service(),
  oneproviderServer: inject.service(),
  commonModals: inject.service(),
  commonLoader: inject.service(),
  session: inject.service(),

  spaces: null,
  validSpaces: computed.filterBy('spaces', 'isLoaded', true),
  spacesSorting: ['isDefault:desc', 'name'],
  // FIXME: spaces -> validSpaces
  validSpacesSorted: computed.sort('spaces', 'spacesSorting'),

  activeSpace: computed.alias('secondaryMenu.activeSpace'),

  // FIXME maybe this implementation was better
  // isLoading: computed('isWorking', 'spaces.length', 'spaces.@each.name', function() {
  //   let { spaces, isWorking } = this.getProperties('spaces', 'isWorking');
  //   return spaces.get('length') === 0 ||
  //     spaces.any(s => !s || !s.get('name')) ||
  //     isWorking;
  // }),

  isLoading: computed('isWorking', 'spaces.@each.isLoaded', function() {
    let { spaces, isWorking } = this.getProperties('spaces', 'isWorking');
    return !spaces ||
      spaces.any(s => !s || !s.get('isLoaded')) ||
      isWorking;
  }),

  isLoadingChanged: observer('isLoading', function() {
    if (this.get('isLoading')) {
      this.setProperties({
        'commonLoader.isLoading': true,
        'commonLoader.message': this.get('i18n').t('components.commonLoader.synchronizingSpaces'),
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

  isCreatingSpace: false,
  newSpaceName: null,

  isJoiningSpace: false,
  joinSpaceToken: null,

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

  spaceToRemove: null,
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

  registerInSecondaryMenu: function() {
    this.set('secondaryMenu.component', this);
  }.on('init'),

  activeSpaceDidChange: function() {
    if (this.get('activeSpace')) {
      this.sendAction('goToSpace', this.get('activeSpace'));
    }
  }.observes('activeSpace'),

  didInsertElement() {
    // reset spaces expanded state
    this.get('spaces').forEach((s) => s.set('isExpanded', false));

    this.isLoadingChanged();
  },

  spaceActionMessage(notifyType, messageId, spaceName) {
    let message = this.get('i18n').t(`components.spacesMenu.notify.${messageId}`, {spaceName: spaceName});
    this.get('notify')[notifyType](message);
  },

  actions: {
    openSubmenuEntry(space, name) {
      console.debug(`spaces-menu: openSubmenuEntry(${space}, ${name})`);
      this.sendAction('openSubmenuEntry', space, name);
    },

    startCreateSpace() {
      this.set('isCreatingSpace', true);
    },

    createSpaceModalOpened() {
      this.setProperties({
        newSpaceName: null,
        isSavingSpace: false
      });
    },

    submitCreateSpace() {
      if (this.get('isCreatingSpace')) {
        this.set('isSavingSpace', true);
        this.send('_submitCreateSpace');
      }
    },

    _submitCreateSpace() {
      // set isSavingSpace one more time, because we can reach this action from input text
      let name = this.get('newSpaceName');
      let s = this.get('store').createRecord('space', {
        name: name,
        hasViewPrivilege: true,
      });
      let savePromise = s.save();
      savePromise.then(
        () => {
          this.get('i18n').t('components.spacesMenu.notify.createSuccess', {
            spaceName: name
          });
        },
        (error) => {
          this.get('notify').error(
            this.get('i18n').t('components.spacesMenu.notify.createFailed', {
              spaceName: name
            }) + ': ' + ((error && error.message) || this.get('i18n').t('common.unknownError'))
          );
          s.deleteRecord();
        }
      );
      savePromise.finally(() => this.setProperties({
        isCreatingSpace: false,
        isSavingSpace: false
      }));
    },

    startJoinSpace() {
      this.set('joinSpaceToken', null);
      this.set('isJoiningSpace', true);
    },

    submitJoinSpace() {
      this.set('isJoiningSpaceWorking', true);
      let token = this.get('joinSpaceToken') && this.get('joinSpaceToken').trim();
      let serverPromise = this.get('oneproviderServer').userJoinSpace(token);
      serverPromise.then(
        (data) => {
          this.spaceActionMessage('info', 'joinSuccess', data.spaceName);
        },
        (errorJson) => {
          console.log(errorJson.message);
          let message = this.get('i18n').t('components.spacesMenu.notify.joinFailed', {errorDetails: errorJson.message});
          this.get('notify').error(message);
        }
      );
      serverPromise.finally(() => {
        this.set('isJoiningSpaceWorking', false);
        this.set('isJoiningSpace', false);
      });
    },

    /*** Single space operation modals ***/

    openSettingsModal(modalName, space) {
      this.set('modalSpace', space);
      this.set('openedModal', modalName);
    },

    setAsHome(space) {
      this.set('isWorking', true);

      let user = this.get('session.user');
      let {id: spaceId, name: spaceName} = space.getProperties('id', 'name');
      console.debug(`Will set new home space to ${spaceId}`);

      user.set('defaultSpaceId', spaceId);

      let savePromise = user.save();

      savePromise.then(() => {
        this.spaceActionMessage('info', 'setAsHomeSuccess', spaceName);
      });

      savePromise.catch(error => {
        this.get('notify').error(
          this.get('i18n').t('components.spacesMenu.notify.setAsHomeFailed', {
            spaceName
          }) + ': ' +
            (error && error.message) || this.get('i18n').t('common.unknownError')
        );
        let reloadUser = user.reload();
        reloadUser.catch(() => {
          console.warn('Reloading User model after alias set failure failed - rolling back local User record');
          user.rollbackAttributes();
        });
      });

      savePromise.finally(() => {
        this.set('isWorking', false);
      });
      
    },

    submitLeaveSpace() {
      try {
        let space = this.get('modalSpace');
        let spaceName = space.get('name');
        this.get('oneproviderServer').userLeaveSpace(space.get('id')).then(
          () => {
            this.spaceActionMessage('info', 'leaveSuccess', spaceName);
          },
          () => {
            this.spaceActionMessage('error', 'leaveFailed', spaceName);
          }
        );
      } finally {
        this.set('modalSpace', null);
        this.set('openedModal', null);
      }
    },

    submitRemoveSpace() {
      try {
        let space = this.get('modalSpace');
        let spaceName = space.get('name');
        space.destroyRecord().then(
          () => {
            this.spaceActionMessage('info', 'removeSuccess', spaceName);
          },
          () => {
            this.spaceActionMessage('error', 'removeFailed', spaceName);
          }
        );
      } finally {
        this.set('modalSpace', null);
        this.set('openedModal', null);
      }
    },
  }
});
