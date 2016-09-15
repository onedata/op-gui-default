import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'tr',

  isSubmitting: false,

  canBeSaved: Ember.computed('newKey', 'newValue', function() {
    return this.get('newKey') && this.get('newValue');
  }),

  changeFocus() {
    if (this.get('newKey')) {
      this.$().find('.basic-new-entry-value')[0].focus();
    } else {
      this.$().find('.basic-new-entry-key')[0].focus();
    }
  },

  save() {
    this.set('isSubmitting');
    const p = new Ember.RSVP.Promise((resolve) => {
      this.sendAction('createNewEntry', this.get('newKey'), this.get('newValue'), resolve);
    });
    p.then(() => {
      this.setProperties({
        newKey: null,
        newValue: null,
      });
    });
    p.finally(() => {
      this.set('isSubmitting', false);
    });
  },

  actions: {
    trySubmit() {
      if (this.get('canBeSaved')) {
        this.save();
      } else {
        this.changeFocus();
      }
    }
  }
});
