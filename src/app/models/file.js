/**
 * Represents a file in a file browser for signed-in user.
 * @module models/file
 * @author Łukasz Opioła
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import DS from 'ember-data';

import FileMixin from 'op-worker-gui/mixins/models/file';

export default DS.Model.extend(FileMixin, {
  share: DS.belongsTo('share', {inverse: null, async: true}),
  parent: DS.belongsTo('file', {inverse: 'children', async: true}),
  children: DS.hasMany('file', {inverse: 'parent', async: true}),
  fileProperty: DS.belongsTo('file-property', {inverse: 'file', async: true}),
  fileAcl: DS.belongsTo('fileAcl', {inverse: 'file', async: true}),
});
