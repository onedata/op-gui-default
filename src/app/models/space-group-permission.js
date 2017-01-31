import DS from 'ember-data';
import permissionModelFactory from 'op-worker-gui/mixin-factories/models/permission';
import FLAG_NAMES from 'op-worker-gui/constants/permission-space-flags';

/**
 * A set of single Space permissions for a single Group
 * @module models/space-group-permission
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(
  permissionModelFactory(FLAG_NAMES, 'space', 'systemGroup')
);
