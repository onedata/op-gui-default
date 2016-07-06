import DS from 'ember-data';

/**
 * FIXME: module doc
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
   * @type AccessControlEntity[]
   */
  acl: DS.attr('array', {defaultValue: []})
});
