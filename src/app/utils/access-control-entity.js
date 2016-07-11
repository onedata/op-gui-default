import Ember from 'ember';
import { MASKS, setAclFlag, getAclFlag } from './acl-utils';

export default Ember.Object.extend({
  // FIXME: use class reopen, because it does many things at object creation!
  init() {
    this._super();

    if (this.get('permissions') == null) {
      let defaultPerms = 0;
      ['read_object', 'write_object', 'read_acl', 'write_acl'].forEach((perm) => {
        defaultPerms = setAclFlag(defaultPerms, perm, true);
      });
      this.set('permissions', defaultPerms);
    }

    // Create computed properties for each mask in form: perm_<mask_name>, eg. perm_read_object
    Object.keys(MASKS).forEach((maskName) => {
      this['perm_' + maskName] = Ember.computed({
        get(/*key*/) {
          return this.hasPermission(maskName);
        },
        set(key, value) {
          this.setPermission(maskName, value);
          return value;
        }
      });
    });
  },


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
  // FIXME: mock
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

  toJSON() {
    return {
      type: this.get('type'),
      subject: this.get('subject'),
      permissions: this.get('permissions'),
      user: this.get('user'),
      group: this.get('group')
    };
  },

  hasPermission(type) {
    return (getAclFlag(this.get('permissions'), type));
  },

  setPermission(type, value) {
    value = (value === undefined ? true : value);
    const newPermissions = setAclFlag(this.get('permissions'), type, value);
    this.set('permissions', newPermissions);
  },

  unsetPermission(type) {
    this.setPermission(type, false);
  }
});
