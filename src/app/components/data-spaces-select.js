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
  commonLoader: Ember.inject.service(),

  /** List of DataSpace records */
  spaces: null,
  validSpaces: function() {
    return this.get('spaces').filter((s) => s.get('isLoaded'));
  }.property('spaces', 'spaces.[]', 'spaces.@each.isLoaded'),
  spacesSorting: ['isDefault:desc', 'name'],
  spacesSorted: Ember.computed.sort('validSpaces', 'spacesSorting'),

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

  /** Space currently selected */
  selectedSpace: Ember.computed.alias('fileSystemTree.selectedSpace'),

  prevSelectedSpace: Ember.computed.alias('fileSystemTree.prevSelectedSpace'),

  selectedSpaceDidChange: function() {
    console.debug(`Spaces Select component: selected space changed to ${this.get('selectedSpace.id')}`);
    if (this.get('selectedSpace')) {
      this.sendAction('goToDataSpace', this.get('selectedSpace.id'));
    }
  }.observes('selectedSpace'),

  didInsertElement() {
    this.isLoadingChanged();
  },

  actions: {
    setSelectedSpace(space) {
      this.set('prevSelectedSpace', this.get('selectedSpace'));
      this.set('selectedSpace', space);
    }
  }
});
