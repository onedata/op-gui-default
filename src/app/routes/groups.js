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
        this.transitionTo('groups.show', group);
      } else {
        this.transitionTo('groups');
      }
    },

    /** Show users/groups/etc. permissions table using route */
    openSubmenuEntry(group, name) {
      console.debug(`route groups: openSubmenuEntry(${group.get('id')}, ${name})`);
      if (group && name) {
        this.transitionTo(`groups.show.${name}`, group);
      }
    }
  }

});
