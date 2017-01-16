import DS from 'ember-data';
import createShareMixin from 'op-worker-gui/mixin-factories/models/share';

/**
 * A container for shared files in public view.
 *
 * @module models/share-public
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(createShareMixin('public'));
