import Ember from 'ember';
import DS from 'ember-data';
import permissionModelFactory from 'op-gui-worker/mixin-factories/models/permission';

const {
  belongsTo
} = DS;

const FLAG_NAMES = [
  'ViewSpace',
  'ModifySpace', 
  'RemoveSpace', 
  'InviteUser', 
  'RemoveUser',
  'InviteGroup', 
  'RemoveGroup', 
  'SetPrivileges', 
  'InviteProvider',
  'RemoveProvider', 
  'ManageShares', 
  'WriteFiles'
];

/**
 * A base DS.Model for creating space permission models.
 * It has persisted attributes based on ``FLAG_NAMES`` in this file, eg.
 * ```
 * permViewSpace: DS.attr('boolean', {defaultValue: false}),
 * ```
 * and runtime properties that indicate flag modification, eg.:
 * ```
 * modViewSpace: false,
 * ```
 * 
 * @module mixins/models/permission-space
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create(permissionModelFactory(FLAG_NAMES), {
  space: belongsTo('space', {async: true}),
});
