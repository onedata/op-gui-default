import Ember from 'ember';
import DS from 'ember-data';
import permissionModelFactory from 'op-gui-worker/mixin-factories/models/permission';
import FLAG_NAMES from 'op-gui-worker/constants/permission-group-flags';

const {
  computed
} = Ember;

const {
  belongsTo
} = DS;

/**
 * A set of single Group permissions for a single (sub)Group
 * Shown on groups/:group_id/users view (beside users permissions table)
 * @module models/group-group-permission
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(permissionModelFactory(FLAG_NAMES), {
  // FIXME
  // systemGroup: belongsTo('systemGroup', {async: true}),

  group: belongsTo('group', {async: true, inverse: null}),

  owner: computed.alias('systemGroup')
});
