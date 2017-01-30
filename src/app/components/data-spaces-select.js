import Ember from 'ember';

const {
  inject,
  computed,
  observer,
  run
} = Ember;

/**
 * Pseudo-selector (specifically, a dropdown) to change data-spaces in data view.
 *
 * Sends actions:
 * - goToDataSpace(space) - should load view associated with data-space;
 *     specifically, loads a root of tree of space directories
 * @module components/data-spaces-select
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  classNames: ['data-spaces-select'],
  fileSystemTree: inject.service(),
  commonLoader: inject.service(),

  /** List of DataSpace records */
  spaces: null,
  validSpaces: computed('spaces', 'spaces.[]', 'spaces.@each.isLoaded', function() {
    return this.get('spaces').filter((s) => s.get('isLoaded'));
  }),
  spacesSorting: ['isDefault:desc', 'name'],
  spacesSorted: computed.sort('validSpaces', 'spacesSorting'),

  isLoading: computed('spaces', 'spaces.length', 'spaces.@each.name', function() {
    return !this.get('spaces.length') || this.get('spaces').any((s) => !s.get('name'));
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

  /** Space currently selected */
  selectedSpace: computed.alias('fileSystemTree.selectedSpace'),

  prevSelectedSpace: computed.alias('fileSystemTree.prevSelectedSpace'),

  selectedSpaceDidChange: observer('selectedSpace.id', function() {
    let selectedSpaceId = this.get('selectedSpace.id');
    console.debug(`Spaces Select component: selected space changed to ${selectedSpaceId}`);
    if (selectedSpaceId) {
      run.scheduleOnce('afterRender', () => {
        this.sendAction('goToDataSpace', selectedSpaceId);
      });
    }
  }),

  init() {
    this._super(...arguments);
    this.isLoadingChanged();
    this.selectedSpaceDidChange();
  },

  actions: {
    setSelectedSpace(space) {
      this.set('prevSelectedSpace', this.get('selectedSpace'));
      this.set('selectedSpace', space);
    }
  }
});
