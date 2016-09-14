import Ember from 'ember';

/**
* Redirect to first share on list if available.
* @module onedata/controllers/shares/index
* @author Jakub Liput
* @copyright (C) 2016 ACK CYFRONET AGH
* @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/
export default Ember.Controller.extend({
  goToDefault() {
    console.debug(`shares.index: Will try to go to first share`);
    let shares = this.get('model');
    if (shares && shares.get('length') > 0) {
      this.transitionToRoute('onedata.shares.show', shares.sortBy('name').get('firstObject'));
    }
  },

  onModelChange: Ember.observer('model.[]', 'model.@each.id', function() {
    this.goToDefault();
  }),
});
