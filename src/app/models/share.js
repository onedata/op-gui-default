import DS from 'ember-data';

/**
 * A container for shared files.
 *
 * @module models/share
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  name: DS.attr('string'),
  file: DS.belongsTo('file', {inverse: null, async: true}),
  containerDir: DS.belongsTo('file-shared', {inverse: null, async: true}),
  dataSpace: DS.belongsTo('data-space', {async: true}),
  publicUrl: DS.attr('string'),
  handle: DS.belongsTo('handle', {async: true}),
});
