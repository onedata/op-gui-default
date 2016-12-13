import Ember from 'ember';

// TODO: doc
export default Ember.Route.extend({
  mainRouteName: 'groups',

  model() {
    return this.store.findAll('group');
  },

  actions: {
    /** Show submenu for Group */
    goToGroup(group) {
      if (group) {
        this.transitionTo('onedata.groups.show', group);
      } else {
        this.transitionTo('onedata.groups');
      }
    },

    /** Show users/groups/etc. permissions table using route */
    openSubmenuEntry(group, name) {
      console.debug(`route groups: openSubmenuEntry(${group.get('id')}, ${name})`);
      if (group && name) {
        this.transitionTo(`onedata.groups.show.${name}`, group);
      }
    }
  }

});
