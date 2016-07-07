import Ember from 'ember';

export default Ember.Component.extend({
  i18n: Ember.inject.service(),

  init() {
    this._super();

    const perm = `ace.perm_${this.get('name')}`;
    this.checked = Ember.computed.alias(perm);
  },

  label: function() {
    return this.get('i18n').t(
      'components.filePermissions.acl.aceItem.permissions.' +
      this.get('name')
    );
  }.property('name'),

  // to inject
  name: null,

  // to inject
  ace: null
});
