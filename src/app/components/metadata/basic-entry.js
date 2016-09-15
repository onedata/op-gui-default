import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'tr',

  actions: {
    remove() {
      this.set('disabled', true);
      const p = new Ember.RSVP.Promise((resolve) => {
        this.sendAction('removeEntry', this.get('key'), resolve);
      });
      p.finally(() => {
        if (this) {
          this.set('disabled', false);
        }
      });
    }
  }
});
