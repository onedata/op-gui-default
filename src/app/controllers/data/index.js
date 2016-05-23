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
  goToDefaultDataSpace() {
    if (this.get('model')) {
      let defaultSpace = this.get('model').find((s) => s.get('isDefault'));
      if (defaultSpace) {
        let dsId = defaultSpace.get('id');
        console.debug(`data.index: Redirecting to default space: ${dsId}`);
        this.transitionToRoute('data.data-space', dsId);
      }
    }
  },

  onDataSpacesChange: function() {
    this.goToDefaultDataSpace();
  }.observes('model.[]'),
});
