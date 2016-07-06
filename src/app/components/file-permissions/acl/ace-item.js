import Ember from 'ember';

export default Ember.Component.extend({
  store: Ember.inject.service(),

  /**
   * @type AccessControlEntity
   */
  ace: null,

  // systemUsers: null,
  // systemGroups: null,
});
