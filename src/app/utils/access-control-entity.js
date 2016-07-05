import Ember from 'ember';

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

let ace = Ember.Object.extend({
  MASKS: function() {
    return MASKS;
  }.property('').readOnly(),

  init() {
    this._super();

    Object.keys(this.get('MASKS')).forEach((maskName) => {
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
  permissions: 0,

  /**
   * Possible values:
   * - ``allow``
   * - ``deny``
   * - ``audit``
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
    const mask = this.get('MASKS')[type];
    return (this.get('permissions') & mask) === mask;
  },

  setPermission(type, value) {
    value = (value === undefined ? true : value);
    const mask = this.get('MASKS')[type];
    const newPermissions = value ?
      (this.get('permissions') | mask) : (this.get('permissions') & ~mask);

    this.set('permissions', newPermissions);
  },

  unsetPermission(type) {
    this.setPermission(type, false);
  }
});

export default ace;
