import Ember from 'ember';

export default Ember.Controller.extend({
  spacesMenu: Ember.inject.service(),

  // FIXME: using this instead of afterMoldel of route, because afterModel did not work properly
  onSpaceChange: function() {
    if (this.get('model.space')) {
      Ember.run.scheduleOnce('afterRender', this, function() {
        console.debug(`controller.spaces: space changed: ${this.get('model.space.id')}`);
        this.set('spacesMenu.activeSpace', this.get('model.space'));

        $('nav.secondary-sidebar').addClass('visible');
      });
    }
  }.observes('model', 'model.space'),
});
