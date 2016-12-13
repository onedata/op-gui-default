/**
 * Represents a file in a public file browser.
 * @module models/file-public
 * @author Łukasz Opioła
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import DS from 'ember-data';

import FileMixin from 'op-worker-gui/mixins/models/file';

export default DS.Model.extend(FileMixin, {
  share: DS.belongsTo('share-public', {inverse: 'file', async: true}),
  parent: DS.belongsTo('file-public', {inverse: 'children', async: true}),
  children: DS.hasMany('file-public', {inverse: 'parent', async: true}),
  fileProperty: DS.belongsTo('file-property-public', {inverse: 'file', async: true}),
});
