import Ember from 'ember';
import DS from 'ember-data';
import permissionModelFactory from 'op-gui-worker/mixin-factories/models/permission';

const {
  belongsTo
} = DS;

const FLAG_NAMES = [
  'ViewGroup',
  'ModifyGroup',
  'SetPrivileges',
  'RemoveGroup',
  'InviteUser',
  'RemoveUser',
  'CreateSpace',
  'JoinSpace',
  'LeaveSpace',
  'GetSupport',
  'InviteGroup',
  'RemoveSubgroup',
  'JoinGroup'
];

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
