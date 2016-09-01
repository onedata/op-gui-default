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
      let firstGroup = groups.filter(g => g.get('id') && g.get('id') != 'null').toArray()[0];
      if (firstGroup) {
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
  onModelChange: function() {
    this.goToFirstGroup();
  }.observes('model.[]', 'model.@each.id')
});
