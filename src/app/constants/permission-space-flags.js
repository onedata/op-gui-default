/**
 * Names of flags for group permissions.
 * 
 * @module constants/permission-group-flags
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
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

// TODO: define properties as read-only

export default FLAG_NAMES;