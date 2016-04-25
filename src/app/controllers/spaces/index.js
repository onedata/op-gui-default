import Ember from 'ember';

export default Ember.Controller.extend({
  goToDefaultSpace() {
    console.debug(`spaces.index: Will try to go to default space`);
    let spaces = this.get('model');
    if (spaces) {
      let defaultSpace = spaces.find((space) => space.get('isDefault'));
      if (defaultSpace) {
        console.debug(`spaces.index: Transition to default space ${defaultSpace.get('id')}`);
        this.transitionToRoute('spaces.show', defaultSpace);
      } else {
        console.debug('spaces.index: No default space found yet');
      }
    }
  },

  /**
    Observe for a default space to appear in spaces route model (which is a spaces list).
    When found - show this space.
    This is a workaround for afterModel, which does not recieve ready spaces list.
  */
  onModelChange: function() {
    this.goToDefaultSpace();
  }.observes('model.[]')
});
