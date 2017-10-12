import Ember from 'ember';
import addConflictLabels from 'ember-cli-onedata-common/utils/add-conflict-labels';

const {
  observer,
  run: {
    debounce
  },
  computed,
  inject: {
    service,
  },
} = Ember;

/**
 * A node in a files tree. Should be a directory.
 *
 * Sends actions:
 * - openDirInBrowser(file) - should open a dir in a browser (data-files-list)
 * @module components/data-files-tree-list
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  fileBrowser: service(),
  session: service(),

  tagName: 'ul',

  classNames: ['data-files-tree-list'],

  /**
   * To inject - a dir which this node represents
   * @required
   * @type {File}
   */
  rootDir: null,

  providerId: computed.alias('session.sessionDetails.providerId'),

  isLoading: computed('rootDir.isLoaded', 'children.@each.isLoaded', function () {
    let children = this.get('children');
    return !this.get('rootDir.isLoaded') || !children.every(c => c.get('isLoaded'));
  }),

  children: computed.alias('rootDir.children'),
  subdirsSorting: ['name:asc'],
  subdirs: computed.filterBy('children', 'isDir', true),
  visibleSubdirs: computed.filter('subdirs', (sd) => sd.get('id') && sd.get('name')),
  visibleSubdirsSorted: computed.sort('visibleSubdirs', 'subdirsSorting'),

  observeProviderLabels: observer(
    'visibleSubdirs.@each.{name,provider}',
    'providerId',
    function () {
      if (this.get('visibleSubdirs').every(sd => sd && sd.isLoaded) && this.get('providerId')) {
        debounce(this, this.updateProviderLabels, 100);
      }
    }
  ),

  updateProviderLabels() {
    const {
      visibleSubdirs,
      providerId
    } = this.getProperties('visibleSubdirs', 'providerId');
    addConflictLabels(visibleSubdirs, 'name', 'provider', providerId);
  },

  actions: {
    /** Pass the action up (action goes up from child dirs) */
    openDirInBrowser(file) {
      this.sendAction('openDirInBrowser', file);
    }
  }
});
