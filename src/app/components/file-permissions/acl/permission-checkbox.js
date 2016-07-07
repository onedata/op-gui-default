import Ember from 'ember';

export default Ember.Component.extend({
  init() {
    this._super();

    const perm = `ace.perm_${this.get('name')}`;
    this.checked = Ember.computed.alias(perm);
  },

  label: Ember.computed.alias('name'),

  // to inject
  name: null,

  // to inject
  ace: null
});
