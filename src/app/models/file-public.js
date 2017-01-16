/**
 * Represents a file in a public file browser.
 * @module models/file-public
 * @author Łukasz Opioła
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import DS from 'ember-data';

import createFileModel from 'op-worker-gui/mixin-factories/models/file';

export default DS.Model.extend(createFileModel('public'));
