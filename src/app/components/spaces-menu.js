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

export default Ember.Component.extend({
  service: Ember.inject.service('spaces-menu'),
  store: Ember.inject.service(),
  notify: Ember.inject.service(),
  oneproviderServer: Ember.inject.service(),
  commonModals: Ember.inject.service(),
  commonLoader: Ember.inject.service(),

  spaces: null,
  validSpaces: Ember.computed.filter('spaces', (s) => s.get('id') && s.get('name')),
  spacesSorting: ['isDefault:desc', 'name'],
  validSpacesSorted: Ember.computed.sort('validSpaces', 'spacesSorting'),

  activeSpace: Ember.computed.alias('service.activeSpace'),

  isLoading: function() {
    return !this.get('spaces.length') || this.get('spaces').any((s) => !s.get('name'));
  }.property('spaces', 'spaces.length', 'spaces.@each.name'),

  isLoadingChanged: function() {
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
  }.observes('isLoading'),

  /*** Variables for actions and modals ***/

  isCreatingSpace: false,
  newSpaceName: null,

  isJoiningSpace: false,
  joinSpaceToken: null,

  spaceToRename: null,
  renameSpaceName: null,

  spaceToRemove: null,

  registerInService: function() {
    this.set('service.component', this);
  }.on('init'),

  clearService: function() {
    this.get('service').clear();
  }.on('willDestroyElement'),

  activeSpaceDidChange: function() {
    if (this.get('activeSpace')) {
      this.sendAction('showSpaceOptions', this.get('activeSpace'));
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
      this.set('newSpaceName', null);
    },

    submitCreateSpace() {
      this.set('isSavingSpace', true);
      try {
        let s = this.get('store').createRecord('space', {
          name: this.get('newSpaceName')
        });
        let savePromise = s.save();
        savePromise.then(
          () => {
            this.set('isSavingSpace', false);
            this.get('i18n').t('components.spacesMenu.notify.createSuccess', {
              spaceName: s.get('name')
            });
          },
          (error) => {
            this.set('isSavingSpace', false);
            this.get('notify').error(
              this.get('i18n').t('components.spacesMenu.notify.createFailed', {
                spaceName: s.get('name')
              }) + ': ' + error
            );
            s.removeRecord();
          }
        );
        savePromise.finally(() => this.set('isCreatingSpace', false));
      } catch (error) {
        this.get('notify').error(`Creating space with name "${this.get('newSpaceName')}" failed`);
        console.error(`Space create failed: ${error}`);
        this.set('isSavingSpace', false);
        this.set('isCreatingSpace', false);
      }
    },

    startJoinSpace() {
      this.set('joinSpaceToken', null);
      this.set('isJoiningSpace', true);
    },

    submitJoinSpace() {
      this.set('isJoiningSpaceWorking', true);
      let token = this.get('joinSpaceToken') && this.get('joinSpaceToken').trim();
      let serverPromise = this.get('oneproviderServer').joinSpace(token);
      serverPromise.then(
        (spaceName) => {
          this.spaceActionMessage('info', 'joinSuccess', spaceName);
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
      this.$().addClass('is-loading');

      let currentHome = this.get('spaces').find((s) => s.get('isDefault'));

      let setNewHome = () => {
        space.set('isDefault', true);
        let savePromise = space.save();
        savePromise.then(
          () => {
            this.spaceActionMessage('info', 'setAsHomeSuccess', space.get('name'));
          },
          (error) => {
            this.get('notify').error(
              this.get('i18n').t('components.spacesMenu.notify.setAsHomeFailed', {
                spaceName: space.get('name')
              }) + ': ' +
                error
            );
          }
        );

        savePromise.finally(() => {
          this.$().removeClass('is-loading');
        });
      };

      if (currentHome) {
        // remove isDefault from current home space
        currentHome.set('isDefault', false);
        let unsetCurrentHomePromise = currentHome.save();
        unsetCurrentHomePromise.then(
          () => {
            setNewHome();
          },
          (error) => {
            this.get('notify').error(
              this.get('i18n').t('components.spacesMenu.notify.setAsHomeFailed', {
                spaceName: space.get('name')
              }) + ': ' +
                error
            );
            this.$().removeClass('is-loading');
          }
        );
      } else {
        // there is some error, because current home should be found
        // anyway, force set new home
        setNewHome();
      }
    },

    submitLeaveSpace() {
      try {
        let space = this.get('modalSpace');
        let spaceName = space.get('name');
        this.get('oneproviderServer').leaveSpace(space).then(
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

    submitRenameSpace() {
      try {
        let space = this.get('modalSpace');
        space.set('name', this.get('renameSpaceName'));
        // TODO: save notification
        space.save();
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
