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
 * @copyright (C) 2016-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  classNames: ['data-spaces-select'],
  fileSystemTree: inject.service(),

  /**
   * List of Spaces records from User relation.
   * Should have ``isLoaded`` computed property.
   */
  spaces: null,
  validSpaces: computed.filterBy('spaces', 'isLoaded', true),
  spacesSorting: ['isDefault:desc', 'name'],
  spacesSorted: computed.sort('validSpaces', 'spacesSorting'),

  isLoading: computed('spaces.isLoaded', 'spaces.@each.isLoaded', function() {
    return this.get('spaces.isLoaded') !== true || this.get('spaces').any((s) => !s.get('isLoaded'));
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
    this.selectedSpaceDidChange();
  },

  actions: {
    setSelectedSpace() {
      return this.get('fileSystemTree').setSelectedSpace(...arguments);
    }
  }
});
