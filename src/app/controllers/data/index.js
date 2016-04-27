import Ember from 'ember';

export default Ember.Controller.extend({
  goToDefaultDataSpace() {
    if (this.get('model')) {
      let defaultSpace = this.get('model').find((s) => s.get('isDefault'));
      if (defaultSpace) {
        let dsId = defaultSpace.get('id');
        console.debug(`data.index: Redirecting to default space: ${dsId}`);
        this.transitionToRoute('data.data-space', dsId);
      }
    }
  },

  onDataSpacesChange: function() {
    this.goToDefaultDataSpace();
  }.observes('model.[]'),
});
