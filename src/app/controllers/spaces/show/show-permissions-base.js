import Ember from 'ember';

export default Ember.Mixin.create({
  spacesMenu: Ember.inject.service(),

  onModelChange: function() {
    let permissionsType = this.get('permissionsType');
    Ember.run.scheduleOnce('afterRender', this, function() {
      this.set('spacesMenu.activeOption', permissionsType);
      $('nav.secondary-sidebar').removeClass('visible');
    });
  }.observes('model')
});
