import Ember from 'ember';

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
  fileSystemTree: Ember.inject.service(),

  /** List of DataSpace records */
  spaces: null,

  /** Space currently selected */
  selectedSpace: function() {
    return this.get('fileSystemTree.selectedSpace');
  }.property('fileSystemTree.selectedSpace'),

  prevSelectedSpace: function() {
    return this.get('fileSystemTree.prevSelectedSpace');
  }.property('fileSystemTree.prevSelectedSpace'),

  selectedSpaceDidChange: function() {
    console.debug(`Spaces Select component: selected space changed to ${this.get('selectedSpace.id')}`);
    if (this.get('selectedSpace')) {
      this.sendAction('goToDataSpace', this.get('selectedSpace.id'));
    }
  }.observes('selectedSpace'),

  actions: {
    setSelectedSpace(space) {
      this.set('prevSelectedSpace', this.get('selectedSpace'));
      this.set('selectedSpace', space);
    }
  }
});
