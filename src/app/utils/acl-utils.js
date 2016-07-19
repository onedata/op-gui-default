/**
 * Contains utils for manipulating ACLs and ACL items (ACE).
 *
 * @module utils/acl-utils
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

const MASKS = {
  read_object:          0x00000001,
  list_container:       0x00000001,
  write_object:         0x00000002,
  add_object:           0x00000002,
  append_data:          0x00000004,
  add_subcontainer:     0x00000004,
  read_metadata:        0x00000008,
  write_metadata:       0x00000010,
  execute:              0x00000020,
  traverse_container:   0x00000020,
  delete_object:        0x00000040,
  delete_subcontainer:  0x00000040,
  read_attributes:      0x00000080,
  write_attributes:     0x00000100,
  delete:               0x00010000,
  read_acl:             0x00020000,
  write_acl:            0x00040000,
  write_owner:          0x00080000,
};

/**
 * Change single flag of ACE.
 * @param {number} permissions Initial permissions to change representation
 * @param {string} flagName One of flag names defined in ``MASKS``
 * @param {boolean} [value=true] A new value of specified flag in permissions
 * @returns {number} a new permissions number values
 */
function setAclFlag(permissions, flagName, value) {
  if (value === undefined) {
    value = true;
  }
  const mask = MASKS[flagName];
  return value ?
    (permissions | mask) : (permissions & ~mask);
}

/**
 * Get value of single flag of ACE.
 * @param {number} permissions Number representation of ACE perms
 * @param {string} flagName Name of permission mask to get (``MASKS``)
 * @returns {boolean} Value of specific permission
 */
function getAclFlag(permissions, flagName) {
  const mask = MASKS[flagName];
  return (permissions & mask) === mask;
}

/**
 * Merges multiple ACLs to one making a sum without duplicate elements.
 * @param {AccessControlEntity[][]} acls
 * @returns {AccessControlEntity[]}
 */
function mergeAcls(acls) {
  // merge lists, remove duplicate elements and remove nulls
  return [].concat.apply([], acls).reduce((newAcl, ace) => {
    if (ace && newAcl.every(e => !ace.compare(e))) {
      newAcl.push(ace);
    }
    return newAcl;
  }, []);
}

export { MASKS, setAclFlag, getAclFlag, mergeAcls };
