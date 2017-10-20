/**
 * A secondary sidebar for selecting Space to show its transfers.
 * Renders list of transfers-menu-item.
 *
 * @module components/transfers-menu
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

import ForceReloadCollectionMixin from 'op-worker-gui/mixins/force-reload-collection';

const {
  observer,
  computed,
  inject,
  computed: { readOnly },
  Component,
} = Ember;

export default Component.extend(ForceReloadCollectionMixin, {
  secondaryMenu: inject.service(),
  store: inject.service(),
  notify: inject.service(),
  oneproviderServer: inject.service(),
  commonModals: inject.service(),
  commonLoader: inject.service(),
  session: inject.service(),

  /**
   * Invoking this function should cause rendering transfers view for given space
   * @virtual
   * @type {Function}
   * @param {Space} space
   * @returns {any} result will be ignored
   */
  goToTransfersForSpace: undefined,
  
  spaces: null,
  collection: readOnly('spaces'),
  
  validSpaces: computed.filterBy('spaces', 'isLoaded', true),
  spacesSorting: ['isDefault:desc', 'name'],
  validSpacesSorted: computed.sort('validSpaces', 'spacesSorting'),

  activeSpace: computed.alias('secondaryMenu.activeSpace'),
  
  // TODO: if in trouble, also assume, that spaces.length === 0 means that isLoading is true
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
  
  activeSpaceDidChange: observer('activeSpace', function() {
    const activeSpace = this.get('activeSpace');
    if (activeSpace) {
      this.goToTransfersForSpace(activeSpace);
    }
  }),

  init() {
    this._super(...arguments);
    this.set('secondaryMenu.component', this);
    this.isLoadingChanged();
  },

  spaceActionMessage(notifyType, messageId, spaceName) {
    let message = this.get('i18n').t(`components.spacesMenu.notify.${messageId}`, {spaceName: spaceName});
    this.get('notify')[notifyType](message);
  },    
});
