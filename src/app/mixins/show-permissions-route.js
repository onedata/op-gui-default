/**
 * An abstract class for routes that show configurable list of permissions.
 *
 * Abstract methods/properties to implement in subclasses:
 * - collectionName - String property - a name of collection from spaces (users or groups)
 *
 * @module mixins/show-permissions-route
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Mixin.create({
  /** Abstract: <string> should be set in subclasses, eg. user, group */
  permissionsType: null,
  /** Abstract: <string> should be set in subclasses, eg. spaces, groups */
  routeType: null,

  permissionsTypeSingular: function() {
    let type = this.get('permissionsType');
    return type.slice(-1) === 's' ? type.slice(0, -1) : type;
  }.property('permissionsType'),

  model() {
    var subject = this.modelFor(`${this.get('routeType')}.show`);
    return {
      subject: subject,
      permissions: subject.get(this.get('collectionName'))
    };
  },

  collectionName: function() {
    return `${this.get('permissionsTypeSingular')}Permissions`;
  }.property('permissionsTypeSingular'),

  onDeactivate: function() {
    Ember.run.scheduleOnce('afterRender', this, function() {
      $('nav.secondary-sidebar').addClass('visible');
    });
  }.on('deactivate'),

  actions: {
    /** Change state of single permission checkbox */
    togglePermission: function(permission, propertyName) {
      var permName = 'perm' + propertyName;
      var modName = 'mod' + propertyName;
      permission.set(permName, !permission.get(permName));
      permission.set(modName, !permission.get(modName));
    },
  }
});
