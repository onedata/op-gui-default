import Ember from 'ember';
import { MASKS, setAclFlag, getAclFlag } from './acl-utils';

/**
 * Exports a class that represents an item of a ACL.
 *
 * Beside properties and functions described in class jsdoc, it has ``perm_*`` properties
 * which internally uses ``permissions`` property by reading/writing flags of number.
 *
 * @module utils/access-control-entity
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

const ACE = Ember.Object.extend(Ember.Comparable, {
  init() {
    this._super();

    if (this.get('permissions') == null) {
      let defaultPerms = 0;
      ['read_object', 'write_object', 'read_acl', 'write_acl'].forEach((perm) => {
        defaultPerms = setAclFlag(defaultPerms, perm, true);
      });
      this.set('permissions', defaultPerms);
    }
  },

  /**
   * If true, the ACE is considered as a new entry, thus it should be allowed to
   * change its subject in GUI.
   * Remember to set this flag to true when creating new ACE in GUI, because
   * by default it is false.
   */
  isCreatedItem: false,

  /**
   * A value containing data about permissions.
   * Specific permissions can be set/get with ``hasPermission``
   * and ``set/unsetPermission`` methods.
   * @type Number
   */
  permissions: null,

  /**
   * Possible values:
   * - ``allow``
   * - ``deny``
   * @type string
   */
  type: 'allow',

  // TODO: userId and groupId in backend - resolve user and group from store

  /**
   * @type Number
   */
  user: null,

  /**
   * @type Number
   */
  group: null,

  /**
   * About "what" the permissions are about.
   * Possible values:
   * - ``user``
   * - ``group``
   * - ``owner``
   * - ``everyone`` (everyone in the space, where the file is)
   * @type string
   */
  subject: 'user',

  /**
   * Return a JSON representation, that is used when serializing object to
   * FilePermission.aclValue array element. Values of object are get from this Ember.Object.
   * @returns {object} backend ACL element containg attributes:
   *  - type
   *  - subject
   *  - permissions
   *  - user
   *  - group
   */
  toJSON() {
    return {
      type: this.get('type'),
      subject: this.get('subject'),
      permissions: this.get('permissions'),
      user: this.get('user'),
      group: this.get('group')
    };
  },

  /**
   * Performs comparison of two ACE elements using properties from JSON representation.
   * @param other Instance of ``AccessControlEntity`` to compare with this
   * @returns {boolean}
   */
  compare(other) {
    return !!other &&
      (this === other || JSON.stringify(this.toJSON()) === JSON.stringify(other.toJSON()));
  },

  /**
   * Checks value of specified permissions flag in ``permissions`` value has a specified
   * Flag names can be found in ``utils/acl-utils`` ``MASKS` object.
   * @param {string} type - name of permission flag to check
   * @returns {boolean} true if this object got a specified permission flag
   */
  hasPermission(type) {
    return (getAclFlag(this.get('permissions'), type));
  },

  /**
   * Modifies a ``permissions`` value to set a permission flag.
   * Flag names can be found in ``utils/acl-utils`` ``MASKS` object.
   * @param {string} type - name of permission flag to set
   * @param {boolean} [value=true] - if true set flag, if false - unset
   */
  setPermission(type, value) {
    value = (value === undefined ? true : value);
    const newPermissions = setAclFlag(this.get('permissions'), type, value);
    this.set('permissions', newPermissions);
  },

  /**
   * Unset a permissions flag - only an alias to ``this.setPermission(type, false)``
   */
  unsetPermission(type) {
    this.setPermission(type, false);
  },

  /**
   * True if this ACL item can be submitted to backend.
   */
  isValid: function() {
    const ps = this.getProperties('subject', 'user', 'group');
    return (ps.subject === 'user' && ps.user) ||
      (ps.subject === 'group' && ps.group);
  }.property('subject', 'user', 'group')
});

let computedPermissions = {};

// Create computed properties for each mask in form: perm_<mask_name>, eg. perm_read_object
Object.keys(MASKS).forEach(function(maskName) {
  computedPermissions['perm_' + maskName] = Ember.computed({
    get(/*key*/) {
      return this.hasPermission(maskName);
    },
    set(key, value) {
      this.setPermission(maskName, value);
      return value;
    }
  });
});

ACE.reopen(computedPermissions);

export default ACE;
