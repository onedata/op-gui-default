import Ember from 'ember';
import DS from 'ember-data';
import PermissionsModelMixin from './permissions-model';

/**
 * A base DS.Model for creating group permission models.
 * @module mixins/permissions-model-group
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/
export default Ember.Mixin.create(PermissionsModelMixin, {
  group: DS.belongsTo('space', {async: true}),

  /* Permission flags - grant permission when true.
   *
   * Note that corresponding mod* flag should be modified on these flag changes.
   */

  permViewGroup: DS.attr('boolean', {defaultValue: false}),
  permModifyGroup: DS.attr('boolean', {defaultValue: false}),
  permSetPrivileges: DS.attr('boolean', {defaultValue: false}),
  permRemoveGroup: DS.attr('boolean', {defaultValue: false}),
  permInviteUser: DS.attr('boolean', {defaultValue: false}),
  permRemoveUser: DS.attr('boolean', {defaultValue: false}),
  permCreateSpace: DS.attr('boolean', {defaultValue: false}),
  permJoinSpace: DS.attr('boolean', {defaultValue: false}),
  permLeaveSpace: DS.attr('boolean', {defaultValue: false}),
  permGetSupport: DS.attr('boolean', {defaultValue: false}),
  permInviteGroup: DS.attr('boolean', {defaultValue: false}),
  permRemoveSubgroup: DS.attr('boolean', {defaultValue: false}),
  permJoinGroup: DS.attr('boolean', {defaultValue: false}),

  /* Modification flags (not persisted) - if true, the corresponding perm*
   * attribte was modified in view but not saved.
   *
   * Note that these flags are not mainained on perm* flags change. Maybe TODO
   */

  modViewGroup: false,
  modModifyGroup: false,
  modSetPrivileges: false,
  modRemoveGroup: false,
  modInviteUser: false,
  modRemoveUser: false,
  modCreateSpace: false,
  modJoinSpace: false,
  modLeaveSpace: false,
  modGetSupport: false,
  modInviteGroup: false,
  modRemoveSubgroup: false,
  modJoinGroup: false,

  /** A collection of permissions and modified flags suffixes,
   *  used mainly to iterate over these flags */
  FLAG_NAMES: [
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
  ],

  // Checks if Permission is modified using mod* flags
  // Returns true if at least one mod* flag is true
  isModified: function() {
    return this.FLAG_NAMES.some(function(flagName) {
      return this.get('mod' + flagName);
    }, this);
  }.property(
    'modViewGroup',
    'modModifyGroup',
    'modSetPrivileges',
    'modRemoveGroup',
    'modInviteUser',
    'modRemoveUser',
    'modCreateSpace',
    'modJoinSpace',
    'modLeaveSpace',
    'modGetSupport',
    'modInviteGroup',
    'modRemoveSubgroup',
    'modJoinGroup'
  ),
});
