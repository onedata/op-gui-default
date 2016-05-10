import Ember from 'ember';
import DS from 'ember-data';
import PermissionsModelSpaceMixin from '../mixins/permissions-model-space';

/**
 * A set of single Group permissions for a single (sub)Group
 * Shown on groups/:group_id/users view (beside users permissions table)
 * @module models/group-group-permission
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(PermissionsModelSpaceMixin, {
  // TODO spaceUser or generic user?
  systemGroup: DS.belongsTo('systemGroup', {async: true}),
  group: DS.belongsTo('group', {async: true}),

  owner: Ember.computed.alias('systemGroup')
});
