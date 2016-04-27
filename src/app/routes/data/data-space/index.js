import Ember from 'ember';

/**
 * Transit to root dir of current data-space.
 * @module routes/data/data-space/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  /** Transit to root dir of current DataSpace */
  afterModel() {
    let dataSpace = this.modelFor('data.data-space');
    let rootDir = dataSpace.get('rootDir');

    if (rootDir && rootDir.get('id')) {
      console.debug(`Redirecting to root dir "${rootDir.get('id')}" of space "${dataSpace.get('id')}"`);
      this.transitionTo('data.data-space.dir', rootDir.get('id'));
    } else {
      console.warn(`Data space "${dataSpace.get('id')}" has no rootDir!`);
    }
  },
});
