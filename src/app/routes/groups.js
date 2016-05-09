import Ember from 'ember';
import MainRouteMixin from '../mixins/main-route-mixin';

// TODO: doc
export default Ember.Route.extend(MainRouteMixin, {
  mainRouteName: 'groups',

  model() {
    return this.store.findAll('group');
  },

  actions: {
    /** Show submenu for Group */
    goToGroup(group) {
      this.transitionTo('groups.show', group);
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
