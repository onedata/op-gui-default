/**
 * Lists a Spaces whose allows to browse files in sub-routes.
 *
 * @module routes/data
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Route.extend({
  fileSystemTree: Ember.inject.service('file-system-tree'),

  mainRouteName: 'data',

  model() {
    return this.store.findAll('data-space');
  },

  afterModel(dataSpaces) {
    this.set('fileSystemTree.spaces', dataSpaces);
  },

  actions: {
    goToDataSpace(spaceId) {
      this.transitionTo('onedata.data.data-space', spaceId);
    }
  }
});
