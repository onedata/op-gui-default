import Ember from 'ember';
import DS from 'ember-data';
import permissionModelFactory from 'op-worker-gui/mixin-factories/models/permission';
import FLAG_NAMES from 'op-worker-gui/constants/permission-space-flags';

const {
  computed
} = Ember;

const {
  belongsTo
} = DS;

/**
 * A set of single Space permissions for a single Group
 * @module models/space-group-permission
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(permissionModelFactory(FLAG_NAMES), {
  // systemGroup: belongsTo('systemGroup', {async: true}),
  // FIXME: implement systemGroup like in space-user-permission

  space: belongsTo('space', {async: true, inverse: null}),

  /** Common alias for owner - in this case group */
  owner: computed.alias('systemGroup'),
});
