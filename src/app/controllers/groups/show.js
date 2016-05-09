import Ember from 'ember';

/**
 * Controller used to watch if model of route changed - and then change active
 * groups menu option.
 * @module controllers/groups/show
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend({
  groupsMenu: Ember.inject.service(),

  onGroupChange: function() {
    if (this.get('model')) {
      Ember.run.scheduleOnce('afterRender', this, function() {
        console.debug(`controller.groups: group changed: ${this.get('model.id')}`);
        this.set('groupsMenu.activeGroup', this.get('model'));

        $('nav.secondary-sidebar').addClass('visible');
      });
    }
  }.observes('model'),
});
