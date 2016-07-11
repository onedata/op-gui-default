import Ember from 'ember';

export default Ember.Component.extend({
  permissions: null,

  init() {
    this._super();
    this.set('modal.posixComponent', this);
  },

  willDestroyElement() {
    this._super();
    this.set('modal.posixComponent', null);
  },

  isReadyToSubmit: function() {
    return !this.get('error');
  }.property('error'),

  actions: {
    submit() {
      // TODO: submitting posix perms form should submit a parent modal
      this.sendAction("submit");
    }
  }
});
