import DS from 'ember-data';
import Ember from 'ember';

/**
 * Holds an Access Control List for File.
 * A relation between ``FileAcl`` and ``File`` is one-to-one and ID of both records
 * are the same. So to find a ``FileAcl`` record for ``File``, simply find a FileAcl record
 * with ``File.get('id')``. Currently there is no ember-data relation defined in both classes.
 *
 * The actual ACL is stored in ``acl`` property, which is an array of
 * AccessControlEntity objects. This property is transformed using acl-array transform.
 *
 * @module models/file-acl
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  /**
   * A file for which permissions is this AC about
   */
  file: DS.belongsTo('file', {inverse: 'fileAcl', async: true}),

  /**
   * @type Ember.Array of AccessControlEntity
   */
  acl: DS.attr('acl-array', {defaultValue: Ember.A()}),

  /**
   * Possible values:
   * - ok - the proper ACL record exists
   * - ne - not exits
   * - ea - eaccess
   * @type {String}
   */
  status: DS.attr('string', {defaultValue: 'ne'}),

  notExists: Ember.computed('status', function() {
    return this.get('status') === 'ne';
  }).readOnly(),

  accessDenied: Ember.computed('status', function() {
    return this.get('status') === 'ea';
  }).readOnly(),

  // HACK: force update of acl attribute as it is not managed by Ember
  // TODO: this should really detect changes in acl
  changedAttributes() {
    return Object.assign(this._super(), {acl: [undefined, this.get('acl')]});
  }
});
