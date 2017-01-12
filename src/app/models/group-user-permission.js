import Ember from 'ember';
import DS from 'ember-data';
import permissionModelFactory from 'op-worker-gui/mixin-factories/models/permission';
import FLAG_NAMES from 'op-worker-gui/constants/permission-group-flags';

const {
  computed
} = Ember;

const {
  belongsTo
} = DS;

/**
 * A set of single Group permissions for a single User
 * Shown on groups/:group_id/users view (beside groups permissions table)
 * @module models/group-user-permission
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(permissionModelFactory(FLAG_NAMES), {
  // FIXME
  // systemUser: belongsTo('systemUser', {async: true}),

  group: belongsTo('group', {async: true, inverse: null}),

  owner: computed.alias('systemUser'),
});
