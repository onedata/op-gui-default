import Ember from 'ember';

/**
 * Controller used to watch if model of route changed - and then change active
 * spaces menu option.
 * @module controllers/spaces/show
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend({
  spacesMenu: Ember.inject.service(),

  onSpaceChange: function() {
    if (this.get('model')) {
      Ember.run.scheduleOnce('afterRender', this, function() {
        console.debug(`controller.spaces: space changed: ${this.get('model.id')}`);
        this.set('spacesMenu.activeSpace', this.get('model'));

        $('nav.secondary-sidebar').addClass('visible');
      });
    }
  }.observes('model'),
});
