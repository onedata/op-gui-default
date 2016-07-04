import DS from 'ember-data';

/**
 * FIXME: module doc
 * @module models/file-access-control
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

export default DS.Model.extend({
  /**
   * Possible values:
   * - ``a`` - allow
   * - ``d`` - deny
   * - ``t`` - audit
   * @type string
   */
  type: DS.attr('string'),

  /**
   * A file for which permissions is this AC about
   */
  file: DS.belongsTo('file', {inverse: 'acl', async: true}),

  /**
   * About "what" the permissions are about.
   * Possible values:
   * - ``u`` - user
   * - ``g`` - group
   * - ``o`` - owner
   * - ``e`` - everyone in the space
   * @type string
   */
  who: DS.attr('string'),

  user: DS.belongsTo('system-user', {inverse: null, async: true}),
  group: DS.belongsTo('system-user', {inverse: null, async: true}),

  permissionsCode: DS.attr('number', {defaultValue: 0}),

  hasPermission(type) {
    const mask = MASKS[type];
    return (this.get('permissionsCode') & mask) === mask;
  },

  setPermission(type) {
    const mask = MASKS[type];
    this.set('permissionsCode', (this.get('permissionsCode') | mask));
  },

  unsetPermission(type) {
    const mask = MASKS[type];
    this.set('permissionsCode', (this.get('permissionsCode') & ~mask));
  }
});
