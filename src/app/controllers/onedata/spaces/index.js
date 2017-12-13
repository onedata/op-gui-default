import Ember from 'ember';
import getDefaultSpace from 'op-worker-gui/utils/get-default-space';

/**
 * Controller used to redirect to default space on model load.
 * Not using afterModel of route, because there were some problems
 * (afterModel was invoked before model load).
 * @module controllers/spaces/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend({
  goToDefaultSpace() {
    console.debug(`controller:onedata.spaces.index: Will try to go to default space`);
    let defaultSpace = getDefaultSpace(this.get('model'));
    if (defaultSpace) {
      this.transitionToRoute('onedata.spaces.show', defaultSpace);
    } else {
      console.debug(`controller:onedata.spaces.index: No space to go`);
    }
  },

  /**
   * Observe for a default space to appear in spaces route model (which is a spaces list).
   * When found - show this space.
   * This is a workaround for afterModel, which does not recieve ready spaces list.
   */
  onModelChange: Ember.observer('model.@each.isLoaded', 'model.isUpdating',
    function() {
      if (this.get('isActive')) {
        this.goToDefaultSpace();
      }
    }
  )
});
