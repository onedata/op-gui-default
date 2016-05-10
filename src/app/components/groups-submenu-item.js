import Ember from 'ember';

/**
 * A item in group submenu, eg. "users" which should transit to users permission
 * table.
 * @module components/groups-submenu-item
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  tagName: 'li',
  classNameBindings: ['typePermissionsClass', 'isActive:active'],

  /** An instance of groups-submenu component - logical parent of this component */
  submenu: null,

  /** group submenu option type: "users" or "groups" */
  type: null,
  typeSingular: function() {
    let type = this.get('type');
    return type.slice(-1) === 's' ? type.slice(0, -1) : type;
  }.property('type'),

  isActive: function() {

    return this.get('submenu.isGroupActive') &&
      this.get('submenu.activeOption') === this.get('type');
  }.property('submenu', 'submenu.activeOption', 'submenu.isGroupActive'),

  typePermissionsClass: function() {
    return `${this.get('type')}-permissions`;
  }.property('type'),

  label: function() {
    return this.get('i18n').t(`components.groupsSubmenu.${this.get('type')}`);
  }.property('type'),

  icon: function() {
    return this.get('typeSingular');
  }.property(),

  click() {
    this.sendAction('action', this.get('type'));
  }
});
