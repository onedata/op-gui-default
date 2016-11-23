import DS from 'ember-data';
import Ember from 'ember';

/**
 * Stores information about file permissions and its type.
 * @module models/file-permission
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  file: DS.belongsTo('file', {inverse: 'filePermission', async: true}),

  /**
   * How permissions for file are handled.
   * Possible values: posix, acl, eaccess
   * * When 'posix': ``posixValue`` property should be not null
   * * When 'acl': ``acl`` property should not be null
   * * When 'eaccess': that means we don't know which type of permissions
   * is used, but give user a possibility to set new permissions
   * 
   * @type {String}
   */
  type: DS.attr('string'),


  // FIXME: hack to work with buggy-backend
  posixValue: DS.attr('number'),
  aclValue: DS.attr('acl-array', {defaultValue: Ember.A()}),

  /**
   * Force request updates of ``posixValues`` and ``aclValue``
   * based on ``type``.
   */
  changedAttributes() {
    let changes = this._super();
    Object.assign(changes, {type: [undefined, this.get('type')]});
    switch (this.get('type')) {
      case 'posix':
        Object.assign(changes, {posixValue: [undefined, this.get('posixValue')]});
        break;
      case 'acl':
        Object.assign(changes, {aclValue: [undefined, this.get('aclValue')]});
        break;
      default:
        break;
    }
    return changes;
  }
});