import Ember from 'ember';

export default Ember.Mixin.create({
  mainMenu: Ember.inject.service(),

  /** Short name of route, eg. "spaces" - must be implemented in subclasses */
  mainRouteName: null,

  onActivate: Ember.on('activate', function() {
    Ember.run.scheduleOnce('afterRender', this, function() {
      this.set('mainMenu.currentItem', this.get('mainRouteName'));
      this.set('mainMenu.isVisible', true);
    });
  }),

});
