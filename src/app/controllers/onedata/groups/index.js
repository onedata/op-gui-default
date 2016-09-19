import Ember from 'ember';

/**
 * Controller used to redirect to default group on model load.
 * Not using afterModel of route, because there were some problems
 * (afterModel was invoked before model load).
 * @module controllers/groups/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend({
  goToFirstGroup() {
    console.debug(`groups.index: Will try to go to default group`);
    let groups = this.get('model');
    if (groups && groups.get('length') > 0) {
      // TODO: which group should be loaded as default?
      const firstGroup = groups.sortBy('name').objectAt(0);
      if (firstGroup && !firstGroup.get('isDeleted') && !firstGroup.get('_invalidRoute')) {
        console.debug(`groups.index: Transition to default group ${firstGroup.get('id')}`);
        this.transitionToRoute('onedata.groups.show', firstGroup);
      } else {
        console.debug('groups.index: No first group found yet');
      }
    }
  },

  /**
    Observe for a default group to appear in groups route model (which is a groups list).
    When found - show this group.
    This is a workaround for afterModel, which does not recieve ready groups list.
  */
  onModelChange: Ember.observer('model.[]', 'model.@each.id', function() {
    if (this.get('isActive')) {
      this.goToFirstGroup();
    }
  })
});
