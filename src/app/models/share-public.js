import DS from 'ember-data';
import Ember from 'ember';

/**
 * A container for shared files in public view.
 *
 * @module models/share-public
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  name: DS.attr('string'),
  file: DS.belongsTo('file-public', {async: true}),
  containerDir: DS.belongsTo('file-public', {async: true}),
  publicUrl: DS.attr('string'),
  handle: DS.belongsTo('handle-public', {inverse: null, async: true}),

  hasHandle: Ember.computed('handle.content', function() {
    return this.belongsTo('handle').id() != null;
  }),
});
