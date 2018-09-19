/**
 * Lists Spaces to browse files in sub-routes.
 *
 * @module routes/data
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import userCollectionModel from 'ember-cli-onedata-common/mixin-factories/routes/user-collection-model';

const {
  inject: {
    service
  }
} = Ember;

export default Ember.Route.extend(userCollectionModel('spaces', { nonEmpty: true }), {
  fileSystemTree: service(),

  mainRouteName: 'data',

  afterModel(dataSpaces) {
    this.set('fileSystemTree.spaces', dataSpaces);
  },

  actions: {
    goToDataSpace(spaceId) {
      if (spaceId) {
        this.replaceWith('onedata.data.data-space', spaceId);
      } else {
        this.replaceWith('onedata.data.index');
      }
    }
  }
});
