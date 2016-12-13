import Ember from 'ember';
import DS from 'ember-data';
import PermissionsModelSpaceMixin from 'op-worker-gui/mixins/permissions-model-space';

/**
 * A set of single Space permissions for a single User
 * @module models/space-user-permission
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(PermissionsModelSpaceMixin, {
  systemUser: DS.belongsTo('systemUser', {async: true}),

  /** Common alias for owner - in this case a user (system-user) */
  owner: Ember.computed.alias('systemUser'),
});
