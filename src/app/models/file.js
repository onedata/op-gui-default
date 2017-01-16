/**
 * Represents a file in a file browser for signed-in user.
 * @module models/file
 * @author Łukasz Opioła
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import DS from 'ember-data';

import createFileModel from 'op-worker-gui/mixin-factories/models/file';

export default DS.Model.extend(createFileModel('regular'), {
  filePermission: DS.belongsTo('file-permission', {inverse: 'file', async: true}),
});
