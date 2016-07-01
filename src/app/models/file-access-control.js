import DS from 'ember-data';

/**
 * FIXME: module doc
 * @module models/file-access-control
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

const MASKS = {
  read_object: 1,
  list_container: 1,
  write_object: 2,
  add_object: 2,
  append_data: 4,
  add_subcontainer: 4,
  read_metadata: 8,
  write_metadata: 10,
  execute: 20,
  traverse_container: 20,
  delete_object: 40,
  delete_subcontainer: 40,
  read_attributes: 80,
  write_attributes: 100,
  delete: 10000,
  read_acl: 20000,
  write_acl: 40000,
  write_owner: 80000,
};

export default DS.Model.extend({
  /*
   * Allowed values: allow, deny, audit
   */
  type: DS.attr('string'),

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

  permissionsCode: DS.attr('number'),

  // TODO: move to utils and write tests
  hasPermission(type) {
    const mask = MASKS[type];
    return (this.permissionsCode & mask) === mask;
  }
});
