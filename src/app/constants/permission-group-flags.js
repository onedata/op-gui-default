/**
 * Names of flags for group permissions.
 * 
 * @module constants/permission-group-flags
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
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
  'InviteGroup',
  'RemoveSubgroup',
  'JoinGroup'
];

// TODO: define properties as read-only

export default FLAG_NAMES;
