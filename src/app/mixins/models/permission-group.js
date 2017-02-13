import Ember from 'ember';
import DS from 'ember-data';
import permissionModelFactory from 'op-worker-gui/mixin-factories/models/permission';

import FLAG_NAMES from 'op-worker-gui/contants/group-flags';

const {
  belongsTo
} = DS;

/**
 * A base DS.Model for creating group permission models.
 * It has persisted attributes based on ``FLAG_NAMES`` in this file, eg.
 * ```
 * permViewGroup: DS.attr('boolean', {defaultValue: false}),
 * ```
 * and runtime properties that indicate flag modification, eg.:
 * ```
 * modViewGroup: false,
 * ```
 * 
 * @module mixins/models/permission-group
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create(permissionModelFactory(FLAG_NAMES), {
  group: belongsTo('group', {async: true}),
});
