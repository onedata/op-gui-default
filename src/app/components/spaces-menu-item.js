import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'li',
  classNames: ['first-level', 'hover-parent'],
  classNameBindings: ['isExpanded:active'],
  setElementId: Ember.on('init', function() {
    this.set('elementId', this.get('space.sidebarEntryId'));
  }),

  spacesMenu: Ember.inject.service(),

  isExpanded: function() {
    return this.get('spacesMenu.activeSpace.id') === this.get('space.id');
  }.property('spacesMenu.activeSpace.id'),

  actions: {
    /** Delegate to goToSpace action, should show submenu to configure Space */
    expand() {
      this.set('spacesMenu.activeSpace', this.get('space'));
    },

    openSubmenuEntry(name) {
      console.debug(`spaces-menu-item: openSubmenuEntry(${name})`);
      this.sendAction('openSubmenuEntry', this.get('space'), name);
    },

    openSettingsModal(modalName, space) {
      this.sendAction('openSettingsModal', modalName, space);
    },

    setAsHome() {
      this.sendAction('setAsHome', this.get('space'));
    }
  }
});
