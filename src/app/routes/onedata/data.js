/**
 * Lists a Spaces whose allows to browse files in sub-routes.
 *
 * @module routes/data
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  inject
} = Ember;

export default Ember.Route.extend({
  fileSystemTree: inject.service(),

  mainRouteName: 'data',

  model() {
    return this.modelFor('onedata').get('spaces');
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
