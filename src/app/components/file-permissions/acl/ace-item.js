import Ember from 'ember';

export default Ember.Component.extend({
  store: Ember.inject.service(),

  init() {
    this._super();
    // FIXME
    // debugger;
  },

  aceChanged: function() {
    // FIXME
    // debugger;
  }.observes('ace', 'ace.subject'),

  /**
   * @type AccessControlEntity
   */
  ace: null,

  // FIXME: translate
  // FIXME: icon
  subjectItems: [
    { id: 'user', text: 'User'},
    { id: 'group', text: 'Group'},
    // Not implemented yet in backend
    // { id: 'owner', text: 'Owner'},
    // { id: 'everyone', text: 'Everyone'},
  ],

  // systemUsers: null,
  // systemGroups: null,
});
