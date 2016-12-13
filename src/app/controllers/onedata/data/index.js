import Ember from 'ember';

/**
 * Watch a data-spaces lodaded into model and try to go to default space when
 * available.
 * @module controllers/data/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend({
  fileSystemTree: Ember.inject.service(),

  onDataSpacesChange: function() {
    this.set('fileSystemTree.spaces', this.get('model'));
  }.observes('model.[]'),
});
