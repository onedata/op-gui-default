import DS from 'ember-data';
import Ember from 'ember';

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
  file: DS.belongsTo('file', {inverse: null, async: true}),

  /**
   * @type Ember.Array of AccessControlEntity
   */
  acl: DS.attr('acl-array', {defaultValue: Ember.A()}),

  // HACK: force update of acl attribute as it is not managed by Ember
  // TODO: this should really detect changes in acl
  changedAttributes() {
    return Object.assign(this._super(), {acl: [undefined, this.get('acl')]});
  }
});
