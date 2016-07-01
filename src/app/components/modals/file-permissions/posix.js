import Ember from 'ember';

export default Ember.Component.extend({
  permissions: null,

  actions: {
    submit() {
      // TODO: submitting posix perms form should submit a parent modal
      this.sendAction("submit");
    }
  }
});
