import Ember from 'ember';
import DS from 'ember-data';
import PermissionsModelSpaceMixin from '../mixins/permissions-model-space';

/**
 * A set of single Group permissions for a single User
 * Shown on groups/:group_id/users view (beside groups permissions table)
 * @module models/group-user-permission
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(PermissionsModelSpaceMixin, {
  // TODO spaceUser or generic user?
  systemUser: DS.belongsTo('systemUser', {async: true}),
  group: DS.belongsTo('group', {async: true}),

  owner: Ember.computed.alias('systemUser'),
});
